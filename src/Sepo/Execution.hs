{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Sepo.Execution where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Aeson (encodeFile, decodeFileStrict)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(UntaggedValue))
import Data.Char (toLower, isAlphaNum)
import Data.Foldable (find, for_)
import Data.Either (partitionEithers)
import Data.List (stripPrefix)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Traversable (for)
import Data.IORef
import Prelude hiding (subtract)
import Sepo.AST
import Sepo.Parser
import System.Environment (getEnv)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified DBus.Client as DBus (Client, connectSession)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Sepo.DBusClient as DBus
import qualified Sepo.WebClient as HTTP
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Monoid (Any(..))
import Control.Monad.Trans.Writer.CPS (runWriterT, tell)
import Control.Monad.Trans.Class (lift)

data Context = Context {
	httpCtx :: HTTP.Ctx,
	dbusClient :: DBus.Client,
	userId :: T.Text,
	userPlaylists :: IORef (Maybe [Playlist]),
	aliasesPath :: FilePath,
	aliases :: IORef (M.Map (Either T.Text T.Text) Cmd),
	cachePath :: FilePath
}

start :: IO Context
start = do
	httpCtx <- HTTP.start
	dbusClient <- DBus.connectSession
	home <- getEnv "HOME"
	let cachePath = home <> "/.cache/sepo"
	createDirectoryIfMissing True $ cachePath <> "/albums"
	createDirectoryIfMissing True $ cachePath <> "/tracks"
	userId <- let path = cachePath <> "/user-id"
		in doesFileExist path >>= \case
			True -> T.readFile path
			False -> do
				user <- HTTP.run_ httpCtx HTTP.getUser
				let userId = HTTP.userId user
				T.writeFile path userId
				pure userId
	userPlaylists <- newIORef Nothing
	let aliasesPath = home <> "/.config/sepo/aliases"
	aliases <- do
		doesFileExist aliasesPath >>= \case
			True -> do
				txt <- T.readFile aliasesPath
				let
					p = fmap M.fromList $ many $ (,) <$> f <* ws <* chunk "=" <* ws <*> cmd5 <* ws <* single ';' <* ws
					f = chunk "spotify:playlist:" *> fmap Left (takeWhileP (Just "playlist id") isAlphaNum) <|> chunk "_" *> fmap Right quoted
				case runParser p aliasesPath txt of
					Left err -> do
						putStrLn $ errorBundlePretty err
						newIORef M.empty
					Right v -> newIORef v
			False -> do
				writeFile aliasesPath ""
				newIORef M.empty
	pure $ Context {..}

type MultiSet a = M.Map a Int
msToL :: MultiSet a -> [a]
msToL ms = M.assocs ms >>= uncurry (flip replicate)

data Playlist = Playlist {
	playlistId :: T.Text,
	playlistName :: T.Text
} deriving (Eq, Ord, Show)

data Artist = Artist {
	artistId :: T.Text,
	artistName :: T.Text
} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions {
	fieldLabelModifier = fmap toLower . fromJust . stripPrefix "artist",
	sumEncoding = UntaggedValue
} 'Artist)

data Album = Album {
	albumId :: T.Text,
	albumName :: T.Text,
	albumArtists :: [Artist]
} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions {
	fieldLabelModifier = fmap toLower . fromJust . stripPrefix "album",
	sumEncoding = UntaggedValue
} 'Album)

data Track = Track {
	trackId :: T.Text,
	trackName :: T.Text,
	trackAlbum :: Album,
	trackArtists :: [Artist],
	trackExplicit :: Bool
} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions {
	fieldLabelModifier = fmap toLower . fromJust . stripPrefix "track",
	sumEncoding = UntaggedValue
} 'Track)

data Tracks = Ordered [Track] | Unordered (MultiSet Track) deriving (Show)
instance Applicative Thunk where
	pure = Strict
	Strict f <*> Strict a = Strict $ f a
	Strict f <*> Lazy a = Lazy $ f <$> a
	Lazy f <*> Strict a = Lazy $ ($ a) <$> f
	Lazy f <*> Lazy a = Lazy $ f <*> a

data Existing = ExPlaylist Playlist | ExAlbum Album | ExArtist Artist deriving (Show)
data Value = Value {
	tracks :: Thunk Tracks,
	existing :: Maybe Existing
}

data Thunk a = Strict a | Lazy (IO a) deriving (Functor)
force :: Thunk a -> IO a
force (Strict v) = pure v
force (Lazy thunk) = thunk
joinThunk :: Thunk (IO a) -> IO (Thunk a)
joinThunk (Strict m) = fmap Strict m
joinThunk (Lazy m) = pure $ Lazy $ join m

tracksList :: Tracks -> [Track]
tracksList (Ordered tracks) = tracks
tracksList (Unordered tracks) = msToL tracks

tracksSet :: Tracks -> MultiSet Track
tracksSet (Ordered tracks) = M.fromListWith (+) $ fmap (, 1) tracks
tracksSet (Unordered tracks) = tracks

httpPlaylist :: HTTP.Playlist -> Playlist
httpPlaylist playlist = Playlist {
		playlistId = HTTP.playlistId playlist,
		playlistName = HTTP.playlistName playlist
	}

httpPlaylistS :: HTTP.PlaylistS -> Playlist
httpPlaylistS playlist = Playlist {
		playlistId = HTTP.playlistSId playlist,
		playlistName = HTTP.playlistSName playlist
	}

httpArtistS :: HTTP.ArtistS -> Artist
httpArtistS artist = Artist {
		artistId = HTTP.artistSId artist,
		artistName = HTTP.artistSName artist
	}

httpAlbumS :: HTTP.AlbumS -> Album
httpAlbumS album = Album {
		albumId = HTTP.albumSId album,
		albumName = HTTP.albumSName album,
		albumArtists = fmap httpArtistS $ HTTP.albumSArtists album
	}

httpAlbum :: HTTP.Album -> Album
httpAlbum album = Album {
		albumId = HTTP.albumId album,
		albumName = HTTP.albumName album,
		albumArtists = fmap httpArtistS $ HTTP.albumArtists album
	}

httpTrackS :: Album -> HTTP.TrackS -> Track
httpTrackS album track = Track {
		trackId = HTTP.trackSId track,
		trackName = HTTP.trackSName track,
		trackAlbum = album,
		trackArtists = fmap httpArtistS $ HTTP.trackSArtists track,
		trackExplicit = HTTP.trackSExplicit track
	}

httpTrack :: HTTP.Track -> Track
httpTrack track = Track {
		trackId = HTTP.trackId track,
		trackName = HTTP.trackName track,
		trackAlbum = httpAlbumS $ HTTP.trackAlbum track,
		trackArtists = fmap httpArtistS $ HTTP.trackArtists track,
		trackExplicit = HTTP.trackExplicit track
	}

findPlaylist :: Context -> T.Text -> IO (Maybe Playlist)
findPlaylist ctx name = do
	-- TODO: only grab the number of pages necessary
	pls <- readIORef (userPlaylists ctx) >>= \case
		Just pls -> pure pls
		Nothing -> do
			pls <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getAllPaged $ HTTP.getPlaylists client
			let pls' = fmap httpPlaylistS pls
			modifyIORef (userPlaylists ctx) $ Just . fromMaybe pls'
			pure pls'
	pure $ find ((== name) . playlistName) pls

executeField :: Context -> Field -> IO Value
executeField ctx (PlaylistId pl_id) = do
	playlist <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getPlaylist client pl_id
	pure $ Value {
		tracks = Lazy $ do
			tracks <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getAllPagedContinue (HTTP.getPlaylistTracks client pl_id) (HTTP.playlistTracks playlist)
			-- TODO: check for a Sepo tag for unordered
			pure $ Ordered $ fmap (httpTrack . HTTP.playlistTrack) tracks
			,
		existing = Just $ ExPlaylist $ httpPlaylist playlist
	}
executeField ctx (PlaylistName name) = do
	pl <- findPlaylist ctx name >>= maybe (fail $ "unknown playlist: " <> T.unpack name) pure
	executeField ctx $ PlaylistId $ playlistId pl
executeField ctx (AliasName name) = do
	alias <- readIORef (aliases ctx) >>= maybe (fail $ "unknown alias: " <> T.unpack name) pure . M.lookup (Right name)
	executeCmd ctx alias
executeField ctx Playing = do
	currentlyPlaying <- HTTP.run_ (httpCtx ctx) HTTP.getCurrentlyPlaying
	context <- maybe (fail "incognito?") pure $ HTTP.currentlyPlayingContext currentlyPlaying
	case HTTP.contextType context of
		HTTP.CTPlaylist -> executeField ctx $ PlaylistId $ (!! 4) $ T.splitOn ":" $ HTTP.contextURI context
		HTTP.CTAlbum -> executeCmd ctx $ AlbumId $ (!! 2) $ T.splitOn ":" $ HTTP.contextURI context
		HTTP.CTArtist -> executeCmd ctx $ ArtistId $ (!! 2) $ T.splitOn ":" $ HTTP.contextURI context

executeFieldAssignment :: Context -> Field -> Cmd -> IO Value
executeFieldAssignment ctx (PlaylistId pl_id) cmd = do
	val <- executeCmd ctx cmd
	playlist <- fmap (>>= find ((== pl_id) . playlistId)) (readIORef $ userPlaylists ctx) >>= \case
		Just playlist -> pure playlist
		Nothing -> fmap httpPlaylist $ HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getPlaylist client pl_id
	tracks <- force $ tracks val
	let chunks = chunksOf 100 $ tracksList tracks
	HTTP.run_ (httpCtx ctx) $ \client -> HTTP.replaceTracks client pl_id $ HTTP.ReplaceTracks $
		fmap (("spotify:track:" <>) . trackId) $ fromMaybe [] $ listToMaybe chunks
	for_ (drop 1 chunks) $ \chunk -> do
		HTTP.run_ (httpCtx ctx) $ \client -> HTTP.addTracks client pl_id $ HTTP.AddTracks
			(fmap (("spotify:track:" <>) . trackId) chunk) Nothing
	T.appendFile (aliasesPath ctx) $ "spotify:playlist:" <> pl_id <> " = " <> reify PPrefix cmd <> ";\n"
	modifyIORef (aliases ctx) $ M.insert (Left pl_id) cmd
	pure $ Value {
		tracks = pure tracks,
		existing = Just $ ExPlaylist playlist
	}
executeFieldAssignment ctx (PlaylistName name) cmd = do
	pl_id <- findPlaylist ctx name >>= \case
		Just pl -> pure $ playlistId pl
		Nothing -> do
			pl <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.createPlaylist client (userId ctx) $ HTTP.CreatePlaylist
				name Nothing Nothing (Just "created by Sepo")
			pure $ HTTP.playlistId pl
	executeFieldAssignment ctx (PlaylistId pl_id) cmd
executeFieldAssignment ctx (AliasName name) cmd = do
	val <- executeCmd ctx cmd
	let
		go nms cmd@(Field (FieldAccess (AliasName nm) [])) = do
			cmd' <- lift $ readIORef (aliases ctx) >>= maybe (fail $ "unknown alias: " <> T.unpack nm) pure . M.lookup (Right nm)
			(cmd'', changed) <- lift $ runWriterT $ go (S.insert nm nms) cmd'
			let changed' = changed <> Any (S.member nm nms)
			tell changed'
			pure $ if getAny changed' then cmd'' else cmd
		go nms (Field (FieldAccess f cmds)) = fmap (Field . FieldAccess f) $ traverse (go nms) cmds
		go nms (Seq a b) = Seq <$> go nms a <*> go nms b
		go nms (Concat a b) = Concat <$> go nms a <*> go nms b
		go nms (Subtract a b) = Subtract <$> go nms a <*> go nms b
		go nms (Intersect a b) = Intersect <$> go nms a <*> go nms b
		go nms (Unique a) = fmap Unique $ go nms a
		go nms (Shuffle a) = fmap Shuffle $ go nms a
		go nms c = pure c
	(cmd', changed) <- runWriterT $ go (S.singleton name) cmd
	T.appendFile (aliasesPath ctx) $ "_" <> reifyQuoted name <> " = " <> reify PPrefix cmd' <> ";\n"
	modifyIORef (aliases ctx) $ M.insert (Right name) cmd'
	pure val
executeFieldAssignment ctx Playing cmd = do
	val <- executeCmd ctx cmd
	case existing val of
		Just (ExPlaylist pl) -> DBus.play (dbusClient ctx) ("spotify:playlist:" <> playlistId pl)
		Just (ExAlbum al) -> DBus.play (dbusClient ctx) ("spotify:album:" <> albumId al)
		Just (ExArtist ar) -> DBus.play (dbusClient ctx) ("spotify:artist:" <> artistId ar)
		Nothing -> fail "TODO: play"
	pure val

executeCmd :: Context -> Cmd -> IO Value
executeCmd ctx (Field (FieldAccess field [])) = executeField ctx field
executeCmd ctx (Field (FieldAccess field [cmd])) = executeFieldAssignment ctx field cmd
executeCmd ctx (Field (FieldAccess field (cmd:cmds))) = do
	executeFieldAssignment ctx field cmd
	executeCmd ctx $ Field $ FieldAccess field cmds
executeCmd ctx (TrackId tr_id) = pure $ Value {
		tracks = Lazy $ do
			let path = cachePath ctx <> "/tracks/" <> T.unpack tr_id
			let get = do
				track <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getTrack client tr_id
				let track' = httpTrack track
				encodeFile path track'
				pure $ Ordered [track']
			doesFileExist path >>= \case
				True -> decodeFileStrict path >>= \case
					Just track -> pure $ Ordered [track]
					Nothing -> get
				False -> get
			,
		existing = Nothing
	}
executeCmd ctx (AlbumId al_id) = do
	let path = cachePath ctx <> "/albums/" <> T.unpack al_id
	let get = do
		album <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getAlbum client al_id
		let album' = httpAlbum album
		encodeFile path album'
		pure (
				album',
				HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getAllPagedContinue
					(HTTP.getAlbumTracks client al_id)
					(HTTP.albumTracks album)
			)
	(album, getTracks) <- doesFileExist path >>= \case
		True -> decodeFileStrict path >>= \case
			Just album -> pure (
					album,
					HTTP.run_ (httpCtx ctx) $ \client ->
						HTTP.getAllPaged $ HTTP.getAlbumTracks client $ albumId album
				)
			Nothing -> get
		False -> get
	pure $ Value {
		tracks = Lazy $ do
			let path = cachePath ctx <> "/albums/" <> T.unpack al_id <> "-tracks"
			let get = do
				tracks <- fmap (fmap (httpTrackS album)) getTracks
				encodeFile path tracks
				pure $ Ordered tracks
			doesFileExist path >>= \case
				True -> decodeFileStrict path >>= \case
					Just tracks -> pure $ Ordered tracks
					Nothing -> get
				False -> get
			,
		existing = Just $ ExAlbum album
	}
executeCmd ctx (ArtistId ar_id) = do
	artist <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getArtist client ar_id
	pure $ Value {
		tracks = Lazy $ do
			albumSs <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getAllPaged $ HTTP.getArtistAlbums client ar_id
			(albumSs, cached) <- fmap partitionEithers $ for albumSs $ \albumS -> do
				let path = cachePath ctx <> "/albums/" <> T.unpack (HTTP.albumSId albumS) <> "-tracks"
				doesFileExist path >>= \case
					True -> decodeFileStrict path >>= \case
						Just tracks -> pure $ Right tracks
						Nothing -> pure $ Left albumS
					False -> pure $ Left albumS
			let albumSBatches = chunksOf 20 albumSs
			albums <- fmap join $ for albumSBatches $ \albumSs -> do
				fmap HTTP.unAlbums $ HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getAlbums client $ HTTP.AlbumIds $ fmap HTTP.albumSId albumSs
			trackss <- for albums $ \album -> do
				tracks <- HTTP.run_ (httpCtx ctx) $ \client -> HTTP.getAllPagedContinue
					(HTTP.getAlbumTracks client $ HTTP.albumId album)
					(HTTP.albumTracks album)
				let album' = httpAlbum album
				let tracks' = fmap (httpTrackS album') tracks
				encodeFile (cachePath ctx <> "/albums/" <> T.unpack (HTTP.albumId album)) album'
				encodeFile (cachePath ctx <> "/albums/" <> T.unpack (HTTP.albumId album) <> "-tracks") tracks'
				pure tracks'
			pure $ Unordered $ M.fromList $ fmap (, 1) $ filter (any ((== ar_id) . artistId) . trackArtists) $ join $ cached ++ trackss
			,
		existing = Just $ ExArtist $ httpArtistS artist
	}
executeCmd ctx PlayingSong = do
	currentlyPlaying <- HTTP.run_ (httpCtx ctx) HTTP.getCurrentlyPlaying
	executeCmd ctx $ TrackId $ HTTP.trackId $ HTTP.currentlyPlayingItem currentlyPlaying
executeCmd ctx Empty = pure $ Value (pure $ Unordered M.empty) Nothing
executeCmd ctx (Seq a b) = executeCmd ctx a *> executeCmd ctx b
executeCmd ctx (Concat a b) = do
	a <- executeCmd ctx a
	b <- executeCmd ctx b
	pure $ flip Value Nothing $ flip fmap ((,) <$> tracks a <*> tracks b) $ \case
		(Ordered a, Ordered b) -> Ordered $ a ++ b
		(Ordered a, Unordered b) -> Ordered $ a ++ msToL b
		(Unordered a, Ordered b) -> Ordered $ msToL a ++ b
		(Unordered a, Unordered b) -> Unordered $ M.unionWith (+) a b
executeCmd ctx (Intersect a b) = do
	a <- executeCmd ctx a
	tracks' <- joinThunk $ flip fmap (tracks a) $ \tracks -> do
		b <- compileFilter ctx b
		pure $ case tracks of
			Ordered tracks -> Ordered $ filter (apply b) tracks
			Unordered tracks -> Unordered $ applyIntersect b tracks
	pure $ a { tracks = tracks' }
executeCmd ctx (Subtract a b) = do
	a <- executeCmd ctx a
	tracks' <- joinThunk $ flip fmap (tracks a) $ \tracks -> do
		b <- compileFilter ctx b
		pure $ case tracks of
			Ordered tracks -> Ordered $ filter (not . apply b) tracks
			Unordered tracks -> Unordered $ applySubtract b tracks
	pure $ a { tracks = tracks' }
executeCmd ctx (Unique cmd) = do
	val <- executeCmd ctx cmd
	pure $ Value (fmap (Unordered . M.map (const 1) . tracksSet) $ tracks val) Nothing
executeCmd ctx (Shuffle a) = fail "TODO: shuffle"

data Filter = Filter {
	filterPosPred :: Track -> Bool,
	filterPosSet :: MultiSet Track,
	filterNegPred :: Track -> Bool,
	filterNegSet :: MultiSet Track
}
instance Semigroup Filter where
	a <> b = Filter {
			filterPosPred = (||) <$> filterPosPred a <*> filterPosPred b,
			filterPosSet = M.union (filterPosSet a) (filterPosSet b),
			filterNegPred = (&&) <$> filterNegPred a <*> filterNegPred b,
			filterNegSet = M.union (M.intersection (filterNegSet a) (filterNegSet b))
				(M.union
					(M.filterWithKey (flip $ const $ filterNegPred a) (filterNegSet b))
					(M.filterWithKey (flip $ const $ filterNegPred b) (filterNegSet a)))
		}
instance Monoid Filter where
	mempty = Filter (const False) M.empty (const True) M.empty

apply :: Filter -> Track -> Bool
apply (Filter {..}) track = filterPosPred track || M.member track filterPosSet || not (filterNegPred track || M.member track filterNegSet)

applyIntersect :: Filter -> M.Map Track a -> M.Map Track a
applyIntersect (Filter {..}) m = M.union
	(M.union
		(M.filterWithKey (flip $ const filterPosPred) m)
		(M.intersection m filterPosSet))
	(M.filterWithKey (flip $ const $ not . filterNegPred) $
		M.difference m filterNegSet)

applySubtract :: Filter -> M.Map Track a -> M.Map Track a
applySubtract (Filter {..}) m = M.union
	(M.filterWithKey (flip $ const $ not . filterPosPred) $
		M.intersection m' filterNegSet)
	(M.filterWithKey (flip $ const $ (&&) <$> filterNegPred <*> not . filterPosPred) m')
	where m' = M.difference m filterPosSet

union :: Filter -> Filter -> Filter
union = (<>)

intersect :: Filter -> Filter -> Filter
intersect a b = Filter {
		filterPosPred = (&&) <$> filterPosPred a <*> filterPosPred b,
		filterPosSet = foldl M.union M.empty [
				M.filterWithKey (flip $ const $ filterPosPred a) (filterPosSet b),
				M.filterWithKey (flip $ const $ filterPosPred b) (filterPosSet a),
				M.intersection (filterPosSet a) (filterPosSet b),
				M.filterWithKey (flip $ const $ not . filterNegPred b) $
					M.difference (filterPosSet a) (filterNegSet b),
				M.filterWithKey (flip $ const $ not . filterNegPred a) $
					M.difference (filterPosSet b) (filterNegSet a)
			],
		filterNegPred = foldl (liftA2 (||)) (const False) [
				(&&) <$> filterNegPred a <*> filterNegPred b,
				(&&) <$> (not <$> filterPosPred a) <*> filterNegPred a,
				(&&) <$> (not <$> filterPosPred b) <*> filterNegPred b
			],
		filterNegSet = foldl M.union M.empty [
				-- M.filterWithKey (flip $ const $ not . filterPosPred a) (filterNegSet a),
				-- M.filterWithKey (flip $ const $ not . filterPosPred b) (filterNegSet b),
				-- M.filterWithKey (flip $ const $ filterNegPred a) (filterNegSet b),
				-- M.filterWithKey (flip $ const $ filterNegPred b) (filterNegSet a),
				M.filterWithKey (flip $ const $ (||) <$> filterNegPred b <*> not . filterPosPred a) (filterNegSet a),
				M.filterWithKey (flip $ const $ (||) <$> filterNegPred a <*> not . filterPosPred b) (filterNegSet b),
				M.intersection (filterNegSet a) (filterNegSet b)
			]
	}

subtract :: Filter -> Filter -> Filter
subtract a b = Filter {
		filterPosPred = const False,
		filterPosSet = M.filterWithKey (flip $ const $ not . filterPosPred b) $ flip M.difference (filterPosSet b) $ foldl M.union M.empty [
				M.filterWithKey (flip $ const $ not . filterNegPred a) $ M.difference (filterNegSet b) (filterNegSet a),
				M.filterWithKey (flip $ const $ filterPosPred a) (filterNegSet b),
				M.filterWithKey (flip $ const $ filterNegPred b) (filterPosSet a),
				M.intersection (filterPosSet a) (filterNegSet b)
			],
		filterNegPred = foldl (liftA2 (||)) (const False) [
				(&&) <$> not . filterPosPred a <*> filterNegPred a,
				not . filterNegPred b,
				filterPosPred b
			],
		filterNegSet = M.union (filterPosSet b)
			(M.filterWithKey (flip $ const $ not . filterPosPred a) (filterNegSet a))
	}

compileFilter :: Context -> Cmd -> IO Filter
compileFilter ctx (Field (FieldAccess (AliasName name) [])) = do
	alias <- readIORef (aliases ctx) >>= maybe (fail $ "unknown alias: " <> T.unpack name) pure . M.lookup (Right name)
	compileFilter ctx alias
compileFilter ctx (TrackId tr_id) = pure $ mempty { filterPosPred = \track -> (== tr_id) $ trackId track }
compileFilter ctx (AlbumId al_id) = pure $ mempty { filterPosPred = \track -> (== al_id) $ albumId $ trackAlbum track }
compileFilter ctx (ArtistId ar_id) = pure $ mempty { filterPosPred = \track -> elem ar_id $ fmap artistId $ trackArtists track }
compileFilter ctx Empty = pure mempty
compileFilter ctx (Seq a b) = executeCmd ctx a >> compileFilter ctx b
compileFilter ctx (Concat a b) = union <$> compileFilter ctx a <*> compileFilter ctx b
compileFilter ctx (Intersect a b) = intersect <$> compileFilter ctx a <*> compileFilter ctx b
compileFilter ctx (Subtract a b) = subtract <$> compileFilter ctx a <*> compileFilter ctx b
compileFilter ctx (Unique cmd) = compileFilter ctx cmd
compileFilter ctx (Shuffle cmd) = compileFilter ctx cmd
compileFilter ctx cmd = do
	val <- executeCmd ctx cmd
	tracks <- force $ tracks val
	pure $ mempty { filterPosSet = tracksSet tracks }
