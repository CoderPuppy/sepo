module Sepo.Runtime.FSCache where

import Conduit ((.|))
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
import Data.Dependent.Sum
import Data.Foldable
import Data.List (uncons, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Traversable
import Data.Tuple (swap)
import Data.Type.Equality ((:~:)(Refl))
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.IORef
import UnliftIO.MVar
import qualified Conduit as Conduit
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
				("#SEPO:PL:SNAPID " <> TL.fromStrict (playlistSnapshotId pl):) $
				("#SEPO:PL:NAME " <> TL.pack (show (playlistName pl)):) $
				("#SEPO:MODE union":) $
				flip fmap tracks $ \(track, addedAt) -> mconcat [
					"spotify:track:",
					TL.fromStrict $ trackId track,
					" #SEPO:PL:ADDEDAT ",
					TL.pack $ show addedAt
				]
			,
		cpDecode = \get bs -> runMaybeT $ do
			(tracks, comments) <- MaybeT $ pure $ either (const Nothing) Just $
				MP.runParser (runWriterT parser) ("cachePlace (SPlaylistTracks " <> show pid <> ")") (T.decodeUtf8 bs)
			[snapshotId] <- pure $ comments >>= (toList . T.stripPrefix "SEPO:PL:SNAPID " . MP.stateInput)
			-- this is also invalidated when the name changes
			-- this is to make it easier to determine what playlist is what from the files
			[name] <- pure $ comments >>= (toList . T.stripPrefix "SEPO:PL:NAME " . MP.stateInput)
			name <- MaybeT $ pure $ readMaybe $ T.unpack name
			pl <- MaybeT $ get $ SPlaylist pid
			guard $ snapshotId == playlistSnapshotId pl
			guard $ name == playlistName pl
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
			Just time <- pure $ T.stripPrefix "SEPO:PL:ADDEDAT " $ MP.stateInput comment
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
		cpEncode = const $ \tracks -> pure $ Just $ TL.encodeUtf8 $ TL.unlines $
			("#SEPO:MODE union":) $
			fmap (("spotify:track:" <>) . TL.fromStrict . trackId) tracks,
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

entriesConduit :: (MonadIO m, Conduit.MonadResource m) => FilePath -> Conduit.ConduitT i (DSum Source (Const FilePath)) m ()
entriesConduit cachePath = do
	do
		let path = cachePath <> "/user-id"
		(doesFileExist path >>=) $ bool (pure ()) $ Conduit.yield (SCurrentUser :=> Const path)
	do
		let dirPath = cachePath <> "/tracks/"
		let dirPath' = T.pack dirPath
		(Conduit.sourceDirectory dirPath .|) $
			Conduit.concatMapMC $ \path -> runMaybeT $ do
				tr_id <- MaybeT $ pure $ T.stripPrefix dirPath' $ T.pack path
				guard $ T.all isAlphaNum tr_id
				pure $ STrack tr_id :=> Const path
	do
		let dirPath = cachePath <> "/albums/"
		let dirPath' = T.pack dirPath
		(Conduit.sourceDirectory dirPath .|) $
			Conduit.concatMapMC $ \path -> runMaybeT $ do
				name <- MaybeT $ pure $ T.stripPrefix dirPath' $ T.pack path
				let (al_id, tracks) = maybe (name, False) (, True) $ T.stripSuffix "-tracks" name
				guard $ T.all isAlphaNum al_id
				pure $ bool ((:=> Const path) . SAlbum) ((:=> Const path) . SAlbumTracks) tracks al_id
	do
		let dirPath = cachePath <> "/playlists/"
		let dirPath' = T.pack dirPath
		(Conduit.sourceDirectory dirPath .|) $
			Conduit.concatMapMC $ \path -> runMaybeT $ do
				pl_id <- MaybeT $ pure $ T.stripPrefix dirPath' $ T.pack path
				guard $ T.all isAlphaNum pl_id
				pure $ SPlaylistTracks pl_id :=> Const path
	do
		let dirPath = cachePath <> "/artists/"
		let dirPath' = T.pack dirPath
		(Conduit.sourceDirectory dirPath .|) $
			Conduit.concatMapMC $ \path -> runMaybeT $ do
				ar_id <- MaybeT $ pure $ T.stripPrefix dirPath' $ T.pack path
				guard $ T.all isAlphaNum ar_id
				pure $ SArtist ar_id :=> Const path

entries :: MonadIO m => FilePath -> m [DSum Source (Const FilePath)]
entries cachePath = execWriterT $ do
	do
		let path = cachePath <> "/user-id"
		(doesFileExist path >>=) $ bool (pure ()) $ tell $ pure $ SCurrentUser :=> Const path
	do
		let dirPath = cachePath <> "/tracks/"
		(listDirectory dirPath >>=) $ traverse_ $ \tr_id ->
			when (all isAlphaNum tr_id) $
				tell $ pure $ STrack (T.pack tr_id) :=> Const (dirPath <> tr_id)
	do
		let dirPath = cachePath <> "/albums/"
		(listDirectory dirPath >>=) $ traverse_ $ \name -> do
			let name' = T.pack name
			let (al_id, tracks) = maybe (name', False) (, True) $ T.stripSuffix "-tracks" name'
			when (T.all isAlphaNum al_id) $
				tell $ pure $ bool
					((:=> Const (dirPath <> name)) . SAlbum)
					((:=> Const (dirPath <> name)) . SAlbumTracks)
					tracks al_id
	do
		let dirPath = cachePath <> "/playlists/"
		(listDirectory dirPath >>=) $ traverse_ $ \pl_id -> do
			when (all isAlphaNum pl_id) $
				tell $ pure $ SPlaylistTracks (T.pack pl_id) :=> Const (dirPath <> pl_id)
	do
		let dirPath = cachePath <> "/artists/"
		(listDirectory dirPath >>=) $ traverse_ $ \ar_id -> do
			when (all isAlphaNum ar_id) $
				tell $ pure $ SArtist (T.pack ar_id) :=> Const (dirPath <> ar_id)
