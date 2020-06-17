{-# LANGUAGE FlexibleContexts #-}

module Sepo.Expr.Runtime where

import Control.Monad.IO.Class
import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (runWriterT, tell)
import Data.Aeson (encodeFile, decodeFileStrict)
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.Foldable (find, for_)
import UnliftIO.IORef
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Any(..))
import Data.Traversable (for)
import Prelude hiding (subtract)
import Sepo.Expr.AST
import Sepo.Expr.Parser
import Sepo.Runtime.Values
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import UnliftIO.Environment (getEnv)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified DBus.Client as DBus (Client, connectSession)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Sepo.DBusClient as DBus
import qualified Sepo.Runtime.Filter as Filter
import qualified Sepo.WebClient as HTTP
import qualified Sepo.Runtime.Query as Query

data Context = Context {
	httpCtx :: HTTP.Ctx,
	dbusClient :: DBus.Client,
	userId :: T.Text,
	userPlaylists :: IORef (Maybe [Playlist]),
	aliasesPath :: FilePath,
	aliases :: IORef (M.Map (Either T.Text T.Text) Cmd)
}

start :: (Query.MonadFraxl Query.Source m, MonadIO m) => HTTP.Ctx -> m Context
start httpCtx = do
	dbusClient <- liftIO DBus.connectSession
	home <- getEnv "HOME"
	userId <- Query.dataFetch $ Query.SCurrentUser
	userPlaylists <- newIORef Nothing
	let aliasesPath = home <> "/.config/sepo/aliases"
	aliases <- do
		doesFileExist aliasesPath >>= \case
			True -> do
				txt <- liftIO $ T.readFile aliasesPath
				let
					p = fmap M.fromList $ many $ (,) <$> f <* ws <* chunk "=" <* ws <*> cmd5 <* ws <* single ';' <* ws
					f = chunk "spotify:playlist:" *> fmap Left (takeWhileP (Just "playlist id") isAlphaNum) <|> chunk "_" *> fmap Right quoted
				case runParser p aliasesPath txt of
					Left err -> do
						liftIO $ putStrLn $ errorBundlePretty err
						newIORef M.empty
					Right v -> newIORef v
			False -> do
				liftIO $ writeFile aliasesPath ""
				newIORef M.empty
	pure $ Context {..}


findPlaylist :: (Query.MonadFraxl Query.Source m, MonadIO m) => Context -> T.Text -> m (Maybe Playlist)
findPlaylist ctx name = do
	-- TODO: only grab the number of pages necessary
	pls <- readIORef (userPlaylists ctx) >>= \case
		Just pls -> pure pls
		Nothing -> do
			pls <- Query.dataFetch $ Query.SCurrentUserPlaylists
			modifyIORef (userPlaylists ctx) $ Just . fromMaybe pls
			pure pls
	pure $ find ((== name) . playlistName) pls

executeField :: (Query.MonadFraxl Query.Source m, MonadIO m, MonadFail m) => Context -> Field -> m (Value m)
executeField ctx (PlaylistId pl_id) = do
	playlist <- Query.dataFetch $ Query.SPlaylist pl_id
	pure $ Value {
		tracks = Lazy $
			-- TODO: check for a Sepo tag for unordered
			fmap (Ordered . fmap fst) $ Query.dataFetch $ Query.SPlaylistTracks pl_id
			,
		existing = Just $ ExPlaylist playlist
	}
executeField ctx (PlaylistName name) = do
	pl <- findPlaylist ctx name >>= maybe (fail $ "unknown playlist: " <> T.unpack name) pure
	executeField ctx $ PlaylistId $ playlistId pl
executeField ctx (AliasName name) = do
	alias <- readIORef (aliases ctx) >>= maybe (fail $ "unknown alias: " <> T.unpack name) pure . M.lookup (Right name)
	executeCmd ctx alias
executeField ctx Playing = do
	(context, _) <- Query.dataFetch $ Query.SCurrentlyPlaying
	(contextType, contextURI) <- maybe (fail "incognito?") pure context
	case contextType of
		HTTP.CTPlaylist -> executeField ctx $ PlaylistId $ (!! 2) $ T.splitOn ":" contextURI
		HTTP.CTAlbum -> executeCmd ctx $ AlbumId $ (!! 2) $ T.splitOn ":" contextURI
		HTTP.CTArtist -> executeCmd ctx $ ArtistId $ (!! 2) $ T.splitOn ":" contextURI

executeFieldAssignment :: (Query.MonadFraxl Query.Source m, MonadIO m, MonadFail m) => Context -> Field -> Cmd -> m (Value m)
executeFieldAssignment ctx (PlaylistId pl_id) cmd = do
	val <- executeCmd ctx cmd
	playlist <- fmap (>>= find ((== pl_id) . playlistId)) (readIORef $ userPlaylists ctx) >>= \case
		Just playlist -> pure playlist
		Nothing -> Query.dataFetch $ Query.SPlaylist pl_id
	tracks <- force $ tracks val
	let chunks = chunksOf 100 $ tracksList tracks
	snapshotId <- fmap HTTP.snapshotRespId $
		HTTP.run_ (httpCtx ctx) $ \client -> HTTP.replaceTracks client pl_id $ HTTP.ReplaceTracks $
		fmap (("spotify:track:" <>) . trackId) $ fromMaybe [] $ listToMaybe chunks
	let addTracks chunk = fmap HTTP.snapshotRespId $
		HTTP.run_ (httpCtx ctx) $ \client -> HTTP.addTracks client pl_id $ HTTP.AddTracks
			(fmap (("spotify:track:" <>) . trackId) chunk) Nothing
	snapshotId <- foldl ((. addTracks) . (*>)) (pure snapshotId) (drop 1 chunks)
	liftIO $ T.appendFile (aliasesPath ctx) $ "spotify:playlist:" <> pl_id <> " = " <> reify PPrefix cmd <> ";\n"
	modifyIORef (aliases ctx) $ M.insert (Left pl_id) cmd
	pure $ Value {
		tracks = pure tracks,
		existing = Just $ ExPlaylist $ playlist { playlistSnapshotId = snapshotId }
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
	liftIO $ T.appendFile (aliasesPath ctx) $ "_" <> reifyQuoted name <> " = " <> reify PPrefix cmd' <> ";\n"
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

executeCmd :: (Query.MonadFraxl Query.Source m, MonadIO m, MonadFail m) => Context -> Cmd -> m (Value m)
executeCmd ctx (Field (FieldAccess field [])) = executeField ctx field
executeCmd ctx (Field (FieldAccess field [cmd])) = executeFieldAssignment ctx field cmd
executeCmd ctx (Field (FieldAccess field (cmd:cmds))) = do
	executeFieldAssignment ctx field cmd
	executeCmd ctx $ Field $ FieldAccess field cmds
executeCmd ctx (TrackId tr_id) = pure $ Value {
		tracks = Lazy $ fmap (Ordered . pure) $ Query.dataFetch $ Query.STrack tr_id,
		existing = Nothing
	}
executeCmd ctx (AlbumId al_id) = do
	album <- Query.dataFetch $ Query.SAlbum al_id
	pure $ Value {
		tracks = Lazy $ fmap Ordered $ Query.dataFetch $ Query.SAlbumTracks al_id,
		existing = Just $ ExAlbum album
	}
executeCmd ctx (ArtistId ar_id) = do
	artist <- Query.dataFetch $ Query.SArtist ar_id
	pure $ Value {
		tracks = Lazy $ do
			albums <- Query.dataFetch $ Query.SArtistAlbums ar_id
			trackss <- for albums $ Query.dataFetch . Query.SAlbumTracks . albumId
			pure $ Unordered $ M.fromList $ fmap (, 1) $ filter (any ((== ar_id) . artistId) . trackArtists) $ join $ trackss
			,
		existing = Just $ ExArtist artist
	}
executeCmd ctx PlayingSong = do
	(_, track) <- Query.dataFetch $ Query.SCurrentlyPlaying
	pure $ Value {
			tracks = Strict $ Ordered [track],
			existing = Nothing
		}
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
			Ordered tracks -> Ordered $ filter (Filter.apply b) tracks
			Unordered tracks -> Unordered $ Filter.applyIntersect b tracks
	pure $ Value tracks' Nothing
executeCmd ctx (Subtract a b) = do
	a <- executeCmd ctx a
	tracks' <- joinThunk $ flip fmap (tracks a) $ \tracks -> do
		b <- compileFilter ctx b
		pure $ case tracks of
			Ordered tracks -> Ordered $ filter (not . Filter.apply b) tracks
			Unordered tracks -> Unordered $ Filter.applySubtract b tracks
	pure $ Value tracks' Nothing
executeCmd ctx (Unique cmd) = do
	val <- executeCmd ctx cmd
	pure $ Value (fmap (Unordered . M.map (const 1) . tracksSet) $ tracks val) Nothing
executeCmd ctx (Shuffle a) = fail "TODO: shuffle"

compileFilter :: (Query.MonadFraxl Query.Source m, MonadIO m, MonadFail m) => Context -> Cmd -> m Filter.Filter
compileFilter ctx (Field (FieldAccess (AliasName name) [])) = do
	alias <- readIORef (aliases ctx) >>= maybe (fail $ "unknown alias: " <> T.unpack name) pure . M.lookup (Right name)
	compileFilter ctx alias
compileFilter ctx (TrackId tr_id) = pure $ mempty { Filter.posPred = \track -> (== tr_id) $ trackId track }
compileFilter ctx (AlbumId al_id) = pure $ mempty { Filter.posPred = \track -> (== al_id) $ albumId $ trackAlbum track }
compileFilter ctx (ArtistId ar_id) = pure $ mempty { Filter.posPred = \track -> elem ar_id $ fmap artistId $ trackArtists track }
compileFilter ctx Empty = pure mempty
compileFilter ctx (Seq a b) = executeCmd ctx a >> compileFilter ctx b
compileFilter ctx (Concat a b) = Filter.union <$> compileFilter ctx a <*> compileFilter ctx b
compileFilter ctx (Intersect a b) = Filter.intersect <$> compileFilter ctx a <*> compileFilter ctx b
compileFilter ctx (Subtract a b) = Filter.subtract <$> compileFilter ctx a <*> compileFilter ctx b
compileFilter ctx (Unique cmd) = compileFilter ctx cmd
compileFilter ctx (Shuffle cmd) = compileFilter ctx cmd
compileFilter ctx cmd = do
	val <- executeCmd ctx cmd
	tracks <- force $ tracks val
	pure $ mempty { Filter.posSet = tracksSet tracks }
