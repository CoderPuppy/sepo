module Sepo.Runtime.FSCache where

import Control.Applicative
import Control.Monad
import Control.Monad.Fraxl
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Data.Bool (bool)
import Data.Char (isAlphaNum)
import Data.Foldable
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Traversable
import Data.Tuple (swap)
import Data.Type.Equality ((:~:)(Refl))
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Directory (doesFileExist)
import UnliftIO.IORef
import UnliftIO.MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Dependent.Map as DM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Sepo.Expr.Parser as Parser
import Sepo.Runtime.Values

data CachePlace a = CachePlace {
	cpPath :: FilePath,
	cpEncode :: forall m. Monad m => (forall b. Source b -> m (Maybe b)) -> a -> m (Maybe BSL.ByteString),
	cpDecode :: forall m. Monad m => (forall b. Source b -> m (Maybe b)) -> BS.ByteString -> m (Maybe a)
}

data CacheSource a where
	CacheSource :: Source a -> CacheSource (Maybe a)
instance GEq CacheSource where
	CacheSource a `geq` CacheSource b = fmap (\Refl -> Refl) $ a `geq` b
instance GCompare CacheSource where
	CacheSource a `gcompare` CacheSource b = case a `gcompare` b of
		GLT -> GLT
		GEQ -> GEQ
		GGT -> GGT

type Cache = DM.DMap CacheSource MVar
type Ctx = (FilePath, IORef Cache)

run :: MonadUnliftIO m => FilePath -> FreerT CacheSource m a -> m a
run cachePath f = do
	cache <- newIORef DM.empty
	runFraxl (fetch_ (cachePath, cache)) f

fetch :: MonadUnliftIO m => FilePath -> Fetch CacheSource m a
fetch cachePath ss = do
	cache <- newIORef DM.empty
	fetch_ (cachePath, cache) ss

fetch_ :: MonadUnliftIO m => Ctx -> Fetch CacheSource m a
fetch_ ctx ss = runConc $ traverseASeq (conc . dispatch ctx) ss

dispatch :: MonadUnliftIO m => Ctx -> CacheSource a -> m (m a)
dispatch (cachePath, cache) (CacheSource s) = case cachePlace s of
	Just place -> do
		var <- newEmptyMVar
		existing <- atomicModifyIORef cache $ swap .
			DM.insertLookupWithKey (\_ _ old -> old) (CacheSource s) var
		pure $ flip fromMaybe (fmap readMVar existing) $ do
			let path = cachePath <> "/" <> cpPath place
			(doesFileExist path >>=) $ bool (pure Nothing) $ do
				bs <- liftIO $ BS.readFile path
				res <- runFraxl (fetch_ (cachePath, cache)) $ cpDecode place (dataFetch . CacheSource) bs
				putMVar var res
				pure res
	Nothing -> pure $ fmap join $ (traverse readMVar =<<) $ fmap (DM.lookup (CacheSource s)) $ readIORef cache

put :: MonadIO m => Ctx -> (forall b. Source b -> m (Maybe b)) -> Bool -> Source a -> a -> m ()
put (cachePath, cache) outside overwrite s v = do
	var <- newMVar $ Just v
	atomicModifyIORef cache $ (, ()) . DM.insert (CacheSource s) var
	case cachePlace s of
		Just place -> do
			let path = cachePath <> "/" <> cpPath place
			shouldWrite <- if overwrite then pure True else
				fmap not $ doesFileExist path
			when shouldWrite $ do
				cpEncode place outside v >>= \case
					Just enc -> liftIO $ BSL.writeFile path enc
					Nothing -> pure ()
		Nothing -> pure ()

cachePlace :: Source a -> Maybe (CachePlace a)
cachePlace SCurrentUser = Just $ CachePlace {
		cpPath = "user-id",
		cpEncode = const $ pure . Just . BSL.fromStrict . T.encodeUtf8,
		cpDecode = const $ pure . Just . T.decodeUtf8
	}
cachePlace SCurrentUserPlaylists = Nothing
-- this must not be cached, the SPlaylistTracks depends on it being the latest version to invalidate the cache
cachePlace (SPlaylist _) = Nothing
-- this depends on SPlaylist not being cached and being prefetched (see Query.exec _ (SPlaylistTracks _))
cachePlace (SPlaylistTracks pid) = Just $ CachePlace {
		cpPath = "playlists/" <> T.unpack pid,
		cpEncode = \get tracks -> runMaybeT $ do
			pl <- MaybeT $ get $ SPlaylist pid
			pure $ TL.encodeUtf8 $ TL.unlines $
				("#SEPO:SNAPID " <> TL.fromStrict (playlistSnapshotId pl):) $
				flip fmap tracks $ \(track, addedAt) -> mconcat [
					"spotify:track:",
					TL.fromStrict $ trackId track,
					" #SEPO:ADDEDAT ",
					TL.pack $ show addedAt
				]
			,
		cpDecode = \get bs -> runMaybeT $ do
			(tracks, comments) <- MaybeT $ pure $ either (const Nothing) Just $
				MP.runParser (runWriterT parser) ("cachePlace (SPlaylistTracks " <> show pid <> ")") (T.decodeUtf8 bs)
			[snapshotId] <- pure $ comments >>= (toList . T.stripPrefix "SEPO:SNAPID " . MP.stateInput)
			pl <- MaybeT $ get $ SPlaylist pid
			guard $ snapshotId == playlistSnapshotId pl
			tracks <- for tracks $ \(pos, (tid, addedAt)) -> do
				guard pos
				track <- MaybeT $ get $ STrack tid
				pure (track, addedAt)
			pure tracks
	}
	where
		parser = Parser.compoundInner' $ do
			options ["spotify:track:", "track:", "tr:", "t:"]
			tid <- MP.takeWhile1P (Just "track id") isAlphaNum
			(_, [comment]) <- listen Parser.wsFlat
			Just time <- pure $ T.stripPrefix "SEPO:ADDEDAT " $ MP.stateInput comment
			Just time <- pure $ readMaybe $ T.unpack time
			pure (tid, time)
cachePlace SCurrentlyPlaying = Nothing
cachePlace (STrack tid) = Just $ CachePlace {
		cpPath = "tracks/" <> T.unpack tid,
		-- TODO: don't need to save id, album or artists
		cpEncode = const $ pure . Just . Aeson.encode,
		cpDecode = const $ pure . Aeson.decodeStrict
	}
cachePlace (SAlbum aid) = Just $ CachePlace {
		cpPath = "albums/" <> T.unpack aid,
		-- TODO: don't need to save id or artists
		cpEncode = const $ pure . Just . Aeson.encode,
		cpDecode = const $ pure . Aeson.decodeStrict
	}
cachePlace (SAlbumTracks aid) = Just $ CachePlace {
		cpPath = "albums/" <> T.unpack aid <> "-tracks",
		cpEncode = const $ pure . Just . TL.encodeUtf8 . TL.unlines .
			fmap (("spotify:track:" <>) . TL.fromStrict . trackId),
		cpDecode = \get bs -> runMaybeT $ do
			(tracks, _) <- MaybeT $ pure $ either (const Nothing) Just $
				MP.runParser (runWriterT parser) ("cachePlace (SAlbumTracks " <> show aid <> ")") (T.decodeUtf8 bs)
			tracks <- for tracks $ \(pos, tid) -> do
				guard pos
				MaybeT $ get $ STrack tid
			pure tracks
	}
	where
		parser = Parser.compoundInner' $ do
			options ["spotify:track:", "track:", "tr:", "t:"]
			MP.takeWhile1P (Just "track id") isAlphaNum
cachePlace (SArtist aid) = Just $ CachePlace {
		cpPath = "artists/" <> T.unpack aid,
		-- TODO: don't need to save id
		cpEncode = const $ pure . Just . Aeson.encode,
		cpDecode = const $ pure . Aeson.decodeStrict
	}
cachePlace (SArtistAlbums _) = Nothing
