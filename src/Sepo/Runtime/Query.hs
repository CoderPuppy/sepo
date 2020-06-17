{-# LANGUAGE TemplateHaskell, FlexibleContexts, BlockArguments #-}

module Sepo.Runtime.Query (
	Ctx(..), start, run, Source(..),
	MonadFraxl(dataFetch), FreerT
) where

import Conduit ((.|))
import qualified Conduit as Conduit
import qualified Data.Conduit.List as Conduit (chunksOf)
import qualified Data.Conduit.ConcurrentMap as Conduit
import Data.Functor.Identity
import Control.Applicative
import Control.Arrow
import Control.Lens ((^.), set)
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.Fraxl
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.State.Class (modify)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Fraxl.Free
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy (execStateT)
import Data.Bool (bool)
import Data.Dependent.Map.Lens
import Data.Foldable
import Data.Functor.Identity (Identity(runIdentity))
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Traversable (for)
import UnliftIO.Async
import UnliftIO.Directory (createDirectoryIfMissing)
import UnliftIO.IORef
import UnliftIO.MVar
import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sepo.Runtime.Values
import Sepo.Runtime.Cache (CacheSource)
import qualified Sepo.Runtime.Cache as Cache
import qualified Sepo.WebClient as HTTP

instance (Applicative f, MonadFail m) => MonadFail (FreeT f m) where
	fail = lift . fail

srcsPlaylists :: ASeq Source a -> S.Set T.Text
srcsPlaylists ANil = S.empty
srcsPlaylists (ACons (SPlaylist pid) t) = S.insert pid $ srcsPlaylists t
srcsPlaylists (ACons _ t) = srcsPlaylists t

srcsTracks :: ASeq Source a -> S.Set T.Text
srcsTracks ANil = S.empty
srcsTracks (ACons (STrack tid) t) = S.insert tid $ srcsTracks t
srcsTracks (ACons _ t) = srcsTracks t

srcsAlbums :: ASeq Source a -> S.Set T.Text
srcsAlbums ANil = S.empty
srcsAlbums (ACons (SAlbum aid) t) = S.insert aid $ srcsAlbums t
srcsAlbums (ACons (SAlbumTracks aid) t) = S.insert aid $ srcsAlbums t
srcsAlbums (ACons _ t) = srcsAlbums t

srcOthers :: Source a -> a -> DM.DMap Source Identity
srcOthers s v = case (s, v) of
	(SCurrentUser, _) -> DM.empty
	(SCurrentUserPlaylists, pls) -> DM.unions $ fmap (\pl -> srcAll (SPlaylist $ playlistId pl) pl) pls
	(SPlaylist pid, _) -> DM.empty
	(SPlaylistTracks pid, trs) -> DM.unions $ fmap (\(tr, _) -> srcAll (STrack $ trackId tr) tr) trs
	(SCurrentlyPlaying, (_, tr)) -> srcAll (STrack $ trackId tr) tr
	(STrack tid, tr) -> DM.union
		(srcAll (SAlbum $ albumId $ trackAlbum tr) (trackAlbum tr))
		(DM.unions $ fmap (\ar -> srcAll (SArtist $ artistId ar) ar) $ trackArtists tr)
	(SAlbum aid, al) -> DM.unions $ fmap (\ar -> srcAll (SArtist $ artistId ar) ar) $ albumArtists al
	(SAlbumTracks aid, trs) -> DM.unions $ fmap (\tr -> srcAll (STrack $ trackId tr) tr) trs
	(SArtist aid, _) -> DM.empty
	(SArtistAlbums aid, als) -> DM.unions $ fmap (\al -> srcAll (SAlbum $ albumId al) al) als

srcAll :: Source a -> a -> DM.DMap Source Identity
srcAll s v = DM.insert s (Identity v) $ srcOthers s v

-- TODO: filesystem cache
data Ctx = Ctx {
	ctxCache :: IORef (DM.DMap Source MVar),
	ctxCachePath :: FilePath,
	-- the tracks caches are for reusing the embedded tracks sections in the general information requests
	-- if they don't exist or are filled with Nothing a plain request will be made
	ctxPlaylistTracksCache :: IORef (M.Map T.Text (MVar (HTTP.Paging HTTP.PlaylistTrack))),
	-- the only reason this should be filled with Nothing is if the general information (SAlbum) is not in the in-memory cache
	-- but is in the filesystem cache, then the batch fetcher will promise to provide it before realizing it doesn't have to make a request
	ctxAlbumTracksCache :: IORef (M.Map T.Text (MVar (Maybe (HTTP.Paging HTTP.TrackS)))),
	ctxFSCache :: IORef Cache.Cache,
	ctxHTTP :: HTTP.Ctx
}

start :: MonadIO m => FilePath -> m Ctx
start ctxCachePath = do
	ctxCache <- newIORef DM.empty
	createDirectoryIfMissing True $ ctxCachePath <> "/playlists"
	createDirectoryIfMissing True $ ctxCachePath <> "/tracks"
	createDirectoryIfMissing True $ ctxCachePath <> "/albums"
	createDirectoryIfMissing True $ ctxCachePath <> "/artists"
	ctxPlaylistTracksCache <- newIORef M.empty
	ctxAlbumTracksCache <- newIORef M.empty
	ctxFSCache <- newIORef DM.empty
	ctxHTTP <- HTTP.start
	pure $ Ctx {..}

cacheGet :: MonadUnliftIO m => Ctx -> Source a -> m (Maybe a)
cacheGet ctx = runFraxl (Cache.fetch_ (ctxCachePath ctx, ctxFSCache ctx)) .  dataFetch . Cache.CacheSource

finalize :: forall m a. MonadUnliftIO m => Ctx -> MVar a -> Source a -> a -> Bool -> m ()
finalize ctx var s res cached = do
	putMVar var res
	others <- DM.traverseWithKey (const $ newMVar . runIdentity) (srcAll s res)
	others' <- atomicModifyIORef (ctxCache ctx) $ \cache -> (DM.union cache others, DM.difference others cache)
	when (not cached) $ do
		cache <- readIORef $ ctxCache ctx
		let
			put :: forall b. Bool -> Source b -> b -> m ()
			put = Cache.put (ctxCachePath ctx) (\s -> traverse readMVar $ DM.lookup s cache)
		runConc $
			(conc (put True s res) *>) $
			DM.forWithKey_ others' $ \s' v' -> conc $ put False s' =<< readMVar v'

fetch :: (MonadUnliftIO m, MonadFail m) => Ctx -> Fetch Source m a
fetch ctx ss = do
	cache <- readIORef $ ctxCache ctx
	playlists <- do
		let pids = S.toList $ S.filter (flip DM.notMember cache . SPlaylist) $ srcsPlaylists ss
		vars <- for pids $ \_ -> newEmptyMVar
		tracksVars <- for pids $ \pid -> if DM.member (SPlaylistTracks pid) cache then pure Nothing else fmap Just newEmptyMVar
		modifyIORef (ctxCache ctx) $ flip (foldl $ flip $ uncurry $ DM.insert . SPlaylist) (zip pids vars)
		modifyIORef (ctxPlaylistTracksCache ctx) $ flip (foldl $ flip $ uncurry $ (fromMaybe id .) . fmap . M.insert) (zip pids tracksVars)
		pure $ forConcurrently_ (zip pids (zip vars tracksVars)) $ \(pid, (var, tracksVar)) -> do
			playlist <- liftIO $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getPlaylist client pid
			putMVar var $ httpPlaylist playlist
			maybe (pure ()) (flip putMVar $ HTTP.playlistTracks playlist) tracksVar
	albums <- do
		let aids = S.toList $ S.filter (flip DM.notMember cache . SAlbum) $ srcsAlbums ss
		vars <- for aids $ \_ -> newEmptyMVar
		tracksVars <- for aids $ \aid -> if DM.member (SAlbumTracks aid) cache then pure Nothing else fmap Just newEmptyMVar
		modifyIORef (ctxCache ctx) $ flip (foldl $ flip $ uncurry $ DM.insert . SAlbum) (zip aids vars)
		modifyIORef (ctxAlbumTracksCache ctx) $ flip (foldl $ flip $ uncurry $ (fromMaybe id .) . fmap . M.insert) (zip aids tracksVars)
		pure $ do
			cacheAsyncs <- fmap S.fromList $ for (zip aids (zip vars tracksVars)) $ \spec@(aid, _) ->
				async $ fmap (spec,) $ cacheGet ctx $ SAlbum aid
			let
				done :: forall m. MonadIO m => (T.Text, (MVar Album, Maybe (MVar (Maybe (HTTP.Paging HTTP.TrackS))))) -> Album -> Maybe (HTTP.Paging HTTP.TrackS) -> m ()
				done (aid, (var, tracksVar)) album paging = do
					maybe (pure ()) (flip putMVar paging) tracksVar
					liftIO $ finalize ctx var (SAlbum aid) album (maybe True (const False) paging)
			let
				producer s | S.null s = pure ()
				producer s = do
					(a, (spec, album)) <- waitAny (S.toList s)
					case album of
						Just album -> done spec album Nothing
						Nothing -> Conduit.yield spec
					producer $ S.delete a s
			Conduit.runResourceT $ Conduit.runConduit $
				(producer cacheAsyncs .|) $
				(Conduit.chunksOf 20 .|) $
				((.|) $ Conduit.concurrentMapM_ 4 10 \chunk -> do
					albums <- liftIO $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAlbums client $ HTTP.AlbumIds $ fmap fst chunk
					for_ (zip chunk (HTTP.unAlbums albums)) $ \((aid, (var, tracksVar)), album) -> do
						bool (fail $ T.unpack $ "incorrect album returned: expected " <> aid <> " got " <> HTTP.albumId album) (pure ()) $
							aid == HTTP.albumId album
						done (aid, (var, tracksVar)) (httpAlbum album) (Just $ HTTP.albumTracks album)
				) $
				Conduit.sinkNull
	tracks <- do
		let tids = S.toList $ S.filter (flip DM.notMember cache . STrack) $ srcsTracks ss
		vars <- for tids $ \_ -> newEmptyMVar
		modifyIORef (ctxCache ctx) $ flip (foldl $ flip $ uncurry $ DM.insert . STrack) (zip tids vars)
		pure $ do
			cacheAsyncs <- fmap S.fromList $ for (zip tids vars) $ \spec@(tid, _) ->
				async $ fmap (spec,) $ cacheGet ctx $ STrack tid
			let
				producer s | S.null s = pure ()
				producer s = do
					(a, ((tid, var), track)) <- waitAny (S.toList s)
					case track of
						Just track -> liftIO $ finalize ctx var (STrack tid) track True
						Nothing -> Conduit.yield (tid, var)
					producer $ S.delete a s
			Conduit.runResourceT $ Conduit.runConduit $
				(producer cacheAsyncs .|) $
				(Conduit.chunksOf 50 .|) $
				((.|) $ Conduit.concurrentMapM_ 4 10 \chunk -> do
					tracks <- liftIO $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getTracks client $ HTTP.TrackIds $ fmap fst chunk
					for_ (zip chunk (HTTP.unTracks tracks)) $ \((tid, var), track) -> do
						bool (fail $ T.unpack $ "incorrect track returned: expected " <> tid <> " got " <> HTTP.trackId track) (pure ()) $
							tid == HTTP.trackId track
						finalize ctx var (STrack tid) (httpTrack track) False
				) $
				Conduit.sinkNull
	prep <- traverseASeq (fmap conc . dispatch ctx) ss
	runConc $
		(conc playlists *>) $
		(conc albums *>) $
		(conc tracks *>) $
		traverseASeq (fmap pure) prep

exec :: (MonadIO m, MonadFail m) => Ctx -> Source a -> m (m a)
exec ctx SCurrentUser = pure $ fmap HTTP.userId $ HTTP.run_ (ctxHTTP ctx) HTTP.getUser
exec ctx SCurrentUserPlaylists = pure $ fmap (fmap httpPlaylistS) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged $ HTTP.getPlaylists client
exec ctx (SPlaylistTracks pid) =
	fmap (fmap $ fmap $ httpTrack . HTTP.playlistTrack &&& HTTP.playlistTrackAddedAt) $
	flip fmap (readIORef $ ctxPlaylistTracksCache ctx) $ (. M.lookup pid) $ \case
	Just paging -> do
		atomicModifyIORef (ctxPlaylistTracksCache ctx) $ (, ()) . M.delete pid
		paging <- takeMVar paging
		HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPagedContinue (HTTP.getPlaylistTracks client pid) paging
	Nothing -> do
		HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged $ HTTP.getPlaylistTracks client pid
exec ctx SCurrentlyPlaying = pure $ fmap (getContext &&& getTrack) $ HTTP.run_ (ctxHTTP ctx) HTTP.getCurrentlyPlaying
	where
		getContext = fmap (HTTP.contextType &&& HTTP.contextURI) . HTTP.currentlyPlayingContext
		getTrack = httpTrack . HTTP.currentlyPlayingItem
exec ctx (SAlbumTracks aid) = do
	album <- fmap (fromJust . DM.lookup (SAlbum aid)) $ readIORef $ ctxCache ctx
	paging <- fmap (M.lookup aid) (readIORef $ ctxAlbumTracksCache ctx)
	let tracks =
		-- fallback to full get
		(maybe (HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged $ HTTP.getAlbumTracks client aid) pure =<<) $
		runMaybeT $ do
			paging <- MaybeT $ pure paging
			atomicModifyIORef (ctxAlbumTracksCache ctx) $ (, ()) . M.delete aid
			paging <- MaybeT $ takeMVar paging
			HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPagedContinue (HTTP.getAlbumTracks client aid) paging
	pure $ (fmap . httpTrackS) <$> readMVar album <*> tracks
exec ctx (SArtist aid) = pure $ fmap httpArtistS $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getArtist client aid
exec ctx (SArtistAlbums aid) = pure $ fmap (fmap httpAlbumS) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged $ HTTP.getArtistAlbums client aid
-- SPlaylist, STrack and SAlbum should be handled elsewhere

dispatch :: forall m a. (MonadUnliftIO m, MonadFail m) => Ctx -> Source a -> m (m a)
dispatch ctx s = fmap (DM.lookup s) (readIORef $ ctxCache ctx) >>= \case
	Just var -> pure $ readMVar var
	Nothing -> do
		var <- newEmptyMVar
		modifyIORef (ctxCache ctx) $ DM.insert s var
		act <- exec ctx s
		pure $ do
			(res, cached) <- cacheGet ctx s >>= \case
				Just res -> pure (res, True)
				Nothing -> fmap (, False) $ act
			finalize ctx var s res cached
			pure res

run :: (MonadUnliftIO m, MonadFail m) => Ctx -> FreerT Source m a -> m a
run ctx = runFraxl $ fetch ctx
