{-# LANGUAGE TemplateHaskell, FlexibleContexts, BlockArguments #-}

module Sepo.Runtime.Query (
	Ctx(..), start, run, Source(..), Action(..), apply, put, evict,
	MonadFraxl(dataFetch), FreerT
) where

import Conduit ((.|))
import Control.Applicative
import Control.Arrow
import Control.Lens ((^.), set)
import Control.Lens.TH (makeLenses)
import Control.Monad
import Control.Monad.Fraxl
import Control.Monad.IO.Class
import Control.Monad.State.Class (modify)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Fraxl.Free
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource (MonadResource(..))
import Control.Monad.Trans.State.Lazy (execStateT)
import Data.Bool (bool)
import Data.Dependent.Map.Lens
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Identity (Identity(runIdentity))
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import Data.Traversable (for)
import Network.HTTP.Types.Status (noContent204)
import Servant.Client (ClientError(UnsupportedContentType), responseStatusCode)
import UnliftIO.Async
import UnliftIO.Directory (createDirectoryIfMissing)
import UnliftIO.IORef
import UnliftIO.MVar
import qualified Conduit as Conduit
import qualified Data.Conduit.ConcurrentMap as Conduit
import qualified Data.Conduit.List as Conduit (chunksOf)
import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sepo.Runtime.FSCache (CacheSource)
import Sepo.Runtime.Values
import qualified Sepo.Runtime.FSCache as FSCache
import qualified Sepo.WebClient as HTTP

instance (Applicative f, MonadFail m) => MonadFail (FreeT f m) where
	fail = lift . fail
instance (Applicative f, MonadResource m) => MonadResource (FreeT f m) where
	liftResourceT = lift . liftResourceT

srcsTracks :: ASeq Source a -> S.Set T.Text
srcsTracks ANil = S.empty
srcsTracks (ACons (STrack tid) t) = S.insert tid $ srcsTracks t
srcsTracks (ACons _ t) = srcsTracks t

-- this also includes SAlbumTracks, to prefetch SAlbum for them
srcsAlbums :: ASeq Source a -> S.Set T.Text
srcsAlbums ANil = S.empty
srcsAlbums (ACons (SAlbum aid) t) = S.insert aid $ srcsAlbums t
srcsAlbums (ACons (SAlbumTracks aid) t) = S.insert aid $ srcsAlbums t
srcsAlbums (ACons _ t) = srcsAlbums t

srcsArtists :: ASeq Source a -> S.Set T.Text
srcsArtists ANil = S.empty
srcsArtists (ACons (SArtist aid) t) = S.insert aid $ srcsArtists t
srcsArtists (ACons _ t) = srcsArtists t

srcOthers :: Source a -> a -> DM.DMap Source Identity
srcOthers s v = case (s, v) of
	(SCurrentUser, _) -> DM.empty
	(SCurrentUserPlaylists, pls) -> DM.unions $ fmap (\pl -> srcAll (SPlaylist $ playlistId pl) pl) pls
	(SCurrentUserArtists, ars) -> DM.unions $ fmap (\ar -> srcAll (SArtist $ artistId ar) ar) ars
	(SCurrentUserAlbums, als) -> DM.unions $ fmap (\(al, _) -> srcAll (SAlbum $ albumId al) al) als
	(SCurrentUserTracks, trs) -> DM.unions $ fmap (\(tr, _) -> srcAll (STrack $ trackId tr) tr) trs
	(SPlaylist pid, _) -> DM.empty
	(SPlaylistTracks pid, trs) -> DM.unions $ fmap (\(tr, _) -> srcAll (STrack $ trackId tr) tr) trs
	(SCurrentlyPlaying, res) -> maybe DM.empty (\(_, tr) -> srcAll (STrack $ trackId tr) tr) res
	(STrack tid, tr) -> DM.union
		(srcAll (SAlbum $ albumId $ trackAlbum tr) (trackAlbum tr))
		(DM.unions $ fmap (\ar -> srcAll (SArtist $ artistId ar) ar) $ trackArtists tr)
	(SAlbum aid, al) -> DM.unions $ fmap (\ar -> srcAll (SArtist $ artistId ar) ar) $ albumArtists al
	(SAlbumTracks aid, trs) -> DM.unions $ fmap (\tr -> srcAll (STrack $ trackId tr) tr) trs
	(SArtist aid, _) -> DM.empty
	(SArtistAlbums aid, als) -> DM.unions $ fmap (\al -> srcAll (SAlbum $ albumId al) al) als
	(SSearchArtists q i, (more, ars)) -> DM.unions $ fmap (\ar -> srcAll (SArtist $ artistId ar) ar) ars
	(SSearchAlbums q i, (more, als)) -> DM.unions $ fmap (\al -> srcAll (SAlbum $ albumId al) al) als
	(SSearchTracks q i, (more, trs)) -> DM.unions $ fmap (\tr -> srcAll (STrack $ trackId tr) tr) trs

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
	ctxFSCache :: IORef FSCache.Cache,
	ctxUseFSCache :: IORef Bool,
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
	ctxUseFSCache <- newIORef True
	ctxHTTP <- HTTP.start
	pure $ Ctx {..}

cacheGet :: MonadIO m => Ctx -> Source a -> m (Maybe a)
cacheGet ctx s = (readIORef (ctxUseFSCache ctx) >>=) $ bool (pure Nothing) $
	runFraxl (FSCache.fetch_ (ctxCachePath ctx, ctxFSCache ctx)) $  dataFetch $ FSCache.CacheSource s

finalize :: forall m a. MonadIO m => Ctx -> MVar a -> Source a -> a -> Bool -> m ()
finalize ctx var s res cached = do
	putMVar var res
	others <- DM.traverseWithKey (const $ newMVar . runIdentity) (srcAll s res)
	others' <- atomicModifyIORef (ctxCache ctx) $ \cache -> (DM.union cache others, DM.difference others cache)
	-- add new items to the FS cache if the data didn't come from the FS cache
	when (not cached) $ do
		cache <- readIORef $ ctxCache ctx
		let
			put :: forall b. Bool -> Source b -> b -> IO ()
			put = FSCache.put (ctxCachePath ctx, ctxFSCache ctx) (\s -> traverse readMVar $ DM.lookup s cache)
		liftIO $ runConc $
			(conc (put True s res) *>) $
			DM.forWithKey_ others' $ \s' v' -> conc $ put False s' =<< readMVar v'

put :: MonadIO m => Ctx -> Source a -> a -> m ()
put ctx s v = do
	var <- newEmptyMVar
	modifyIORef (ctxCache ctx) $ DM.insert s var
	finalize ctx var s v False

evict :: MonadIO m => Ctx -> Source a -> m ()
evict ctx s = modifyIORef (ctxCache ctx) $ DM.delete s

fetch :: (MonadIO m, MonadFail m) => Ctx -> Fetch Source m a
fetch ctx ss = do
	evict ctx SCurrentlyPlaying
	cache <- readIORef $ ctxCache ctx
	-- this is prefetched to allow batching and to be used for later SAlbumTracks
	albums <- do
		let aids = S.toList $ S.filter (flip DM.notMember cache . SAlbum) $ srcsAlbums ss
		vars <- for aids $ \_ -> newEmptyMVar
		tracksVars <- for aids $ \aid -> if DM.member (SAlbumTracks aid) cache then pure Nothing else fmap Just newEmptyMVar
		modifyIORef (ctxCache ctx) $ flip (foldl $ flip $ uncurry $ DM.insert . SAlbum) (zip aids vars)
		modifyIORef (ctxAlbumTracksCache ctx) $ flip (foldl $ flip $ uncurry $ maybe id . M.insert) (zip aids tracksVars)
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
	-- this is prefetched to allow batching
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
	-- this is prefetched to allow batching
	artists <- do
		let aids = S.toList $ S.filter (flip DM.notMember cache . SArtist) $ srcsArtists ss
		vars <- for aids $ \_ -> newEmptyMVar
		modifyIORef (ctxCache ctx) $ flip (foldl $ flip $ uncurry $ DM.insert . SArtist) (zip aids vars)
		pure $ do
			cacheAsyncs <- fmap S.fromList $ for (zip aids vars) $ \spec@(aid, _) ->
				async $ fmap (spec,) $ cacheGet ctx $ SArtist aid
			let
				producer s | S.null s = pure ()
				producer s = do
					(a, ((aid, var), artist)) <- waitAny (S.toList s)
					case artist of
						Just artist -> liftIO $ finalize ctx var (SArtist aid) artist True
						Nothing -> Conduit.yield (aid, var)
					producer $ S.delete a s
			Conduit.runResourceT $ Conduit.runConduit $
				(producer cacheAsyncs .|) $
				(Conduit.chunksOf 50 .|) $
				((.|) $ Conduit.concurrentMapM_ 4 10 \chunk -> do
					artists <- liftIO $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getArtists client $ HTTP.ArtistIds $ fmap fst chunk
					for_ (zip chunk (HTTP.unArtists artists)) $ \((aid, var), artist) -> do
						bool (fail $ T.unpack $ "incorrect artist returned: expected " <> aid <> " got " <> HTTP.artistSId artist) (pure ()) $
							aid == HTTP.artistSId artist
						finalize ctx var (SArtist aid) (httpArtistS artist) False
				) $
				Conduit.sinkNull
	prep <- traverseASeq (fmap conc . liftIO . dispatch ctx) ss
	liftIO $ runConc $
		(conc albums *>) $
		(conc tracks *>) $
		(conc artists *>) $
		traverseASeq (fmap pure) prep

exec :: (MonadIO m, MonadFail m) => Ctx -> Source a -> m (m (), m a)
exec ctx SCurrentUser = pure $ (pure (),) $ fmap HTTP.userId $ HTTP.run_ (ctxHTTP ctx) HTTP.getUser
exec ctx SCurrentUserPlaylists = pure $ (pure (),) $ fmap (fmap httpPlaylistS) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged (HTTP.getMyPlaylists client) (Just 50)
exec ctx SCurrentUserArtists = pure $ (pure (),) $ fmap (fmap httpArtistS) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged ((fmap HTTP.myArtists .) . HTTP.getMyArtists client "artist") (Just 50)
exec ctx SCurrentUserAlbums = pure $ (pure (),) $ fmap (fmap (httpAlbum . HTTP.savedAlbum &&& HTTP.savedAlbumAddedAt)) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged (HTTP.getMyAlbums client) (Just 50)
exec ctx SCurrentUserTracks = pure $ (pure (),) $ fmap (fmap (httpTrack . HTTP.savedTrack &&& HTTP.savedTrackAddedAt)) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged (HTTP.getMyTracks client) (Just 50)
exec ctx (SPlaylist pid) = do
	tracksVar <-
		(fmap (DM.member $ SPlaylistTracks pid) (readIORef $ ctxCache ctx) >>=) $
		flip bool (pure Nothing) $ do
			var <- newEmptyMVar
			modifyIORef (ctxPlaylistTracksCache ctx) $ M.insert pid var
			pure $ Just var
	pure $ (pure (),) $ do
		playlist <- liftIO $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getPlaylist client pid
		maybe (pure ()) (flip putMVar $ HTTP.playlistTracks playlist) tracksVar
		pure $ httpPlaylist playlist
exec ctx (SPlaylistTracks pid) = do
	-- this prefetches SPlaylist pid for the FS cache, see FSCache.cachePlace (SPlaylistTracks _)
	playlist <- dispatch ctx $ SPlaylist pid
	paging <- fmap (M.lookup pid) (readIORef $ ctxPlaylistTracksCache ctx)
	pure $ (void playlist,) $
		fmap (fmap $ httpTrack . HTTP.playlistTrack &&& HTTP.playlistTrackAddedAt) $
		-- fallback to full get
		(maybe (HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged (HTTP.getPlaylistTracks client pid) (Just 100)) pure =<<) $
		runMaybeT $ do
			atomicModifyIORef (ctxPlaylistTracksCache ctx) $ (, ()) . M.delete pid
			paging <- MaybeT $ pure paging
			paging <- takeMVar paging
			HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPagedContinue (HTTP.getPlaylistTracks client pid) (Just 100) paging
exec ctx SCurrentlyPlaying = pure $ (pure (),) $ do
	res <- HTTP.run (ctxHTTP ctx) HTTP.getCurrentlyPlaying
	res <- case res of
		Left (UnsupportedContentType _ res) | responseStatusCode res == noContent204 ->
			pure Nothing
		Left err -> fail $ show err
		Right v -> pure (Just v)
	pure $ fmap (getContext &&& getTrack) res
	where
		getContext = fmap (HTTP.contextType &&& HTTP.contextURI) . HTTP.currentlyPlayingContext
		getTrack = httpTrack . HTTP.currentlyPlayingItem
exec ctx (STrack aid) = fail "BROKEN: STrack should be prefetched"
exec ctx (SAlbum aid) = fail "BROKEN: SAlbum should be prefetched"
exec ctx (SAlbumTracks aid) = do
	album <- fmap (fromJust . DM.lookup (SAlbum aid)) $ readIORef $ ctxCache ctx
	paging <- fmap (M.lookup aid) (readIORef $ ctxAlbumTracksCache ctx)
	let tracks =
		-- fallback to full get
		(maybe (HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged (HTTP.getAlbumTracks client aid) (Just 50)) pure =<<) $
		runMaybeT $ do
			atomicModifyIORef (ctxAlbumTracksCache ctx) $ (, ()) . M.delete aid
			paging <- MaybeT $ pure paging
			paging <- MaybeT $ takeMVar paging
			HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPagedContinue (HTTP.getAlbumTracks client aid) (Just 50) paging
	pure $ (pure (),) $ (fmap . httpTrackS) <$> readMVar album <*> tracks
exec ctx (SArtist aid) = fail "BROKEN: SArtist should be prefetched"
exec ctx (SArtistAlbums aid) = pure $ (pure (),) $ fmap (fmap httpAlbumS) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged (HTTP.getArtistAlbums client aid) (Just 50)
exec ctx (SSearchArtists q i) = pure $ (pure (),) $ fmap ((isJust . HTTP.pagingNext &&& fmap httpArtistS . HTTP.pagingItems) . HTTP.unSearchArtists) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.searchArtists client HTTP.SearchTypeArtist q (Just $ i * 50) (Just 50)
exec ctx (SSearchAlbums q i) = pure $ (pure (),) $ fmap ((isJust . HTTP.pagingNext &&& fmap httpAlbum . HTTP.pagingItems) . HTTP.unSearchAlbums) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.searchAlbums client HTTP.SearchTypeAlbum q (Just $ i * 50) (Just 50)
exec ctx (SSearchTracks q i) = pure $ (pure (),) $ fmap ((isJust . HTTP.pagingNext &&& fmap httpTrack . HTTP.pagingItems) . HTTP.unSearchTracks) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.searchTracks client HTTP.SearchTypeTrack q (Just $ i * 50) (Just 50)

dispatch :: forall m a. (MonadIO m, MonadFail m) => Ctx -> Source a -> m (m a)
dispatch ctx s = fmap (DM.lookup s) (readIORef $ ctxCache ctx) >>= \case
	Just var -> pure $ readMVar var
	Nothing -> do
		var <- newEmptyMVar
		modifyIORef (ctxCache ctx) $ DM.insert s var
		(act1, act) <- exec ctx s
		pure $ do
			act1
			(res, cached) <- cacheGet ctx s >>= \case
				Just res -> pure (res, True)
				Nothing -> fmap (, False) $ act
			finalize ctx var s res cached
			pure res

run :: (MonadIO m, MonadFail m) => Ctx -> FreerT Source m a -> m a
run ctx = runFraxl $ fetch ctx

apply :: (MonadIO m, MonadFail m) => Ctx -> Action a -> m a
apply ctx (APlaylistCreate info) = do
	userId <- run ctx $ dataFetch SCurrentUser
	pl <- fmap httpPlaylist $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.createPlaylist client userId info
	put ctx (SPlaylist $ playlistId pl) pl
	put ctx (SPlaylistTracks $ playlistId pl) []
	pure pl
apply ctx (APlaylistReplace pid tracks) = do
	pl <- run ctx $ dataFetch $ SPlaylist pid
	snapshotId <- fmap HTTP.snapshotRespId $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.replaceTracks client pid $
		HTTP.ReplaceTracks $ fmap (("spotify:track:" <>) . trackId) tracks
	pl <- pure $ pl { playlistSnapshotId = snapshotId }
	put ctx (SPlaylist $ playlistId pl) pl
	-- TODO: can this be a put
	evict ctx $ SPlaylistTracks $ playlistId pl
	pure pl
apply ctx (APlaylistAdd pid place tracks) = do
	pl <- run ctx $ dataFetch $ SPlaylist pid
	snapshotId <- fmap HTTP.snapshotRespId $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.addTracks client pid $
		HTTP.AddTracks (fmap (("spotify:track:" <>) . trackId) tracks) place
	pl <- pure $ pl { playlistSnapshotId = snapshotId }
	put ctx (SPlaylist $ playlistId pl) pl
	-- TODO: can this be a put
	evict ctx $ SPlaylistTracks $ playlistId pl
	pure pl
apply ctx (APlaylistRemove pid snap_id tracks) = do
	pl <- run ctx $ dataFetch $ SPlaylist pid
	snapshotId <- fmap HTTP.snapshotRespId $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.removeTracks client pid $
		flip HTTP.RemoveTracks snap_id $ flip fmap (M.toList tracks) $ \(track, idxs) ->
			HTTP.RemoveTrack ("spotify:track:" <> trackId track) idxs
	pl <- pure $ pl { playlistSnapshotId = snapshotId }
	put ctx (SPlaylist $ playlistId pl) pl
	-- TODO: can this be a put
	evict ctx $ SPlaylistTracks $ playlistId pl
	pure pl
apply ctx (ASaveTracks tracks) = do
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.saveTracks client $ HTTP.TrackIds $ fmap trackId tracks
	evict ctx SCurrentUserTracks
	pure ()
apply ctx (AUnsaveTracks tracks) = do
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.unsaveTracks client $ HTTP.TrackIds $ fmap trackId tracks
	evict ctx SCurrentUserTracks
	pure ()
