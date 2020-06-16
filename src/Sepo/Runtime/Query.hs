{-# LANGUAGE TemplateHaskell #-}

module Sepo.Runtime.Query (
	Ctx(..), start, run, Source(..),
	MonadFraxl(dataFetch), FreerT
) where

import Control.Monad.Trans.Class
import Control.Applicative
import Control.Arrow
import Control.Lens ((^.), set)
import Control.Lens.TH (makeLenses)
import Control.Monad.Fraxl
import Control.Monad.Trans.Fraxl.Free
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.State.Class (modify)
import Control.Monad.Trans.State.Lazy (execStateT)
import Data.Bool (bool)
import Data.Dependent.Map.Lens
import Data.Foldable
import Data.Functor.Identity (Identity(runIdentity))
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust)
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import Data.Type.Equality ((:~:)(Refl))
import UnliftIO.Async (async, wait, forConcurrently_, Conc, conc, runConc)
import UnliftIO.IORef
import UnliftIO.MVar
import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sepo.Runtime.Values
import qualified Sepo.WebClient as HTTP

instance (Applicative f, MonadFail m) => MonadFail (FreeT f m) where
	fail = lift . fail

data Source a where
	SCurrentUser :: Source T.Text
	SCurrentUserPlaylists :: Source [Playlist]
	SPlaylist :: T.Text -> Source Playlist
	SPlaylistTracks :: T.Text -> Source [(Track, UTCTime)]
	SCurrentlyPlaying :: Source (Maybe (HTTP.ContextType, T.Text), Track)
	STrack :: T.Text -> Source Track
	SAlbum :: T.Text -> Source Album
	SAlbumTracks :: T.Text -> Source [Track]
	SArtist :: T.Text -> Source Artist
	SArtistAlbums :: T.Text -> Source [Album]
instance GEq Source where
	SCurrentUser `geq` SCurrentUser = Just Refl
	SCurrentUserPlaylists `geq` SCurrentUserPlaylists = Just Refl
	SPlaylist a `geq` SPlaylist b = bool (Just Refl) Nothing $ a == b
	SPlaylistTracks a `geq` SPlaylistTracks b = bool (Just Refl) Nothing $ a == b
	SCurrentlyPlaying `geq` SCurrentlyPlaying = Just Refl
	STrack a `geq` STrack b = bool (Just Refl) Nothing $ a == b
	SAlbum a `geq` SAlbum b = bool (Just Refl) Nothing $ a == b
	SAlbumTracks a `geq` SAlbumTracks b = bool (Just Refl) Nothing $ a == b
	SArtist a `geq` SArtist b = bool (Just Refl) Nothing $ a == b
	SArtistAlbums a `geq` SArtistAlbums b = bool (Just Refl) Nothing $ a == b
	_ `geq` _ = Nothing
instance GCompare Source where
	SCurrentUser `gcompare` SCurrentUser = GEQ
	SCurrentUser `gcompare` _ = GLT
	SPlaylist _ `gcompare` SCurrentUser = GGT
	SPlaylist a `gcompare` SPlaylist b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SPlaylist _ `gcompare` _ = GLT
	SPlaylistTracks _ `gcompare` SCurrentUser = GGT
	SPlaylistTracks _ `gcompare` SPlaylist _ = GGT
	SPlaylistTracks a `gcompare` SPlaylistTracks b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SPlaylistTracks _ `gcompare` _ = GLT
	SCurrentlyPlaying `gcompare` SCurrentUser = GGT
	SCurrentlyPlaying `gcompare` SPlaylist _ = GGT
	SCurrentlyPlaying `gcompare` SPlaylistTracks _ = GGT
	SCurrentlyPlaying `gcompare` SCurrentlyPlaying = GEQ
	SCurrentlyPlaying `gcompare` _ = GLT
	STrack _ `gcompare` SCurrentUser = GGT
	STrack _ `gcompare` SPlaylist _ = GGT
	STrack _ `gcompare` SPlaylistTracks _ = GGT
	STrack _ `gcompare` SCurrentlyPlaying = GGT
	STrack a `gcompare` STrack b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	STrack _ `gcompare` _ = GLT
	SAlbum _ `gcompare` SCurrentUser = GGT
	SAlbum _ `gcompare` SPlaylist _ = GGT
	SAlbum _ `gcompare` SPlaylistTracks _ = GGT
	SAlbum _ `gcompare` SCurrentlyPlaying = GGT
	SAlbum _ `gcompare` STrack _ = GGT
	SAlbum a `gcompare` SAlbum b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SAlbum _ `gcompare` _ = GLT
	SAlbumTracks _ `gcompare` SCurrentUser = GGT
	SAlbumTracks _ `gcompare` SPlaylist _ = GGT
	SAlbumTracks _ `gcompare` SPlaylistTracks _ = GGT
	SAlbumTracks _ `gcompare` SCurrentlyPlaying = GGT
	SAlbumTracks _ `gcompare` STrack _ = GGT
	SAlbumTracks _ `gcompare` SAlbum _ = GGT
	SAlbumTracks a `gcompare` SAlbumTracks b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SAlbumTracks _ `gcompare` _ = GLT
	SArtist _ `gcompare` SCurrentUser = GGT
	SArtist _ `gcompare` SPlaylist _ = GGT
	SArtist _ `gcompare` SPlaylistTracks _ = GGT
	SArtist _ `gcompare` SCurrentlyPlaying = GGT
	SArtist _ `gcompare` STrack _ = GGT
	SArtist _ `gcompare` SAlbum _ = GGT
	SArtist _ `gcompare` SAlbumTracks _ = GGT
	SArtist a `gcompare` SArtist b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SArtist _ `gcompare` _ = GLT
	SArtistAlbums _ `gcompare` SCurrentUser = GGT
	SArtistAlbums _ `gcompare` SPlaylist _ = GGT
	SArtistAlbums _ `gcompare` SPlaylistTracks _ = GGT
	SArtistAlbums _ `gcompare` SCurrentlyPlaying = GGT
	SArtistAlbums _ `gcompare` STrack _ = GGT
	SArtistAlbums _ `gcompare` SAlbum _ = GGT
	SArtistAlbums _ `gcompare` SAlbumTracks _ = GGT
	SArtistAlbums _ `gcompare` SArtist _ = GGT
	SArtistAlbums a `gcompare` SArtistAlbums b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SArtistAlbums _ `gcompare` _ = GLT

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

-- TODO: filesystem cache
data Ctx = Ctx {
	ctxCache :: IORef (DM.DMap Source MVar),
	ctxPlaylistTracksCache :: IORef (M.Map T.Text (MVar (HTTP.Paging HTTP.PlaylistTrack))),
	ctxAlbumTracksCache :: IORef (M.Map T.Text (MVar (HTTP.Paging HTTP.TrackS))),
	ctxHTTP :: HTTP.Ctx
}

start :: MonadIO m => m Ctx
start = do
	ctxCache <- newIORef DM.empty
	ctxPlaylistTracksCache <- newIORef M.empty
	ctxAlbumTracksCache <- newIORef M.empty
	ctxHTTP <- HTTP.start
	pure $ Ctx {..}

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
		pure $ forConcurrently_ (chunksOf 20 (zip aids (zip vars tracksVars))) $ \chunk -> do
			albums <- liftIO $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAlbums client $ HTTP.AlbumIds $ fmap fst chunk
			for_ (zip chunk (HTTP.unAlbums albums)) $ \((aid, (var, tracksVar)), album) -> do
				bool (fail $ T.unpack $ "incorrect album returned: expected " <> aid <> " got " <> HTTP.albumId album) (pure ()) $
					aid == HTTP.albumId album
				putMVar var $ httpAlbum album
				maybe (pure ()) (flip putMVar $ HTTP.albumTracks album) tracksVar
	tracks <- do
		let tids = S.toList $ S.filter (flip DM.notMember cache . STrack) $ srcsTracks ss
		vars <- for tids $ \_ -> newEmptyMVar
		modifyIORef (ctxCache ctx) $ flip (foldl $ flip $ uncurry $ DM.insert . STrack) (zip tids vars)
		pure $ forConcurrently_ (chunksOf 50 (zip tids vars)) $ \chunk -> do
			tracks <- liftIO $ HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getTracks client $ HTTP.TrackIds $ fmap fst chunk
			for_ (zip chunk (HTTP.unTracks tracks)) $ \((tid, var), track) -> do
				bool (fail $ T.unpack $ "incorrect track returned: expected " <> tid <> " got " <> HTTP.trackId track) (pure ()) $
					tid == HTTP.trackId track
				putMVar var $ httpTrack track
	runConc $
		(conc playlists *>) $
		(conc albums *>) $
		(conc tracks *>) $
		($ ss) $ traverseASeq $ conc . dispatch ctx

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
	tracks <- flip fmap (readIORef $ ctxAlbumTracksCache ctx) $ (. M.lookup aid) $ \case
		Just paging -> do
			atomicModifyIORef (ctxAlbumTracksCache ctx) $ (, ()) . M.delete aid
			paging <- takeMVar paging
			HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPagedContinue (HTTP.getAlbumTracks client aid) paging
		Nothing -> do
			HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged $ HTTP.getAlbumTracks client aid
	pure $ (fmap . httpTrackS) <$> readMVar album <*> tracks
exec ctx (SArtist aid) = pure $ fmap httpArtistS $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getArtist client aid
exec ctx (SArtistAlbums aid) = pure $ fmap (fmap httpAlbumS) $
	HTTP.run_ (ctxHTTP ctx) $ \client -> HTTP.getAllPaged $ HTTP.getArtistAlbums client aid
-- SPlaylist, STrack and SAlbum should be handled elsewhere

dispatch :: (MonadIO m, MonadFail m) => Ctx -> Source a -> m (m a)
dispatch ctx s = fmap (DM.lookup s) (readIORef $ ctxCache ctx) >>= \case
	Just var -> pure $ readMVar var
	Nothing -> do
		var <- liftIO newEmptyMVar
		modifyIORef (ctxCache ctx) $ DM.insert s var
		act <- exec ctx s
		pure $ do
			res <- act
			putMVar var res
			pure res

run :: (MonadUnliftIO m, MonadFail m) => Ctx -> FreerT Source m a -> m a
run ctx = runFraxl $ fetch ctx
