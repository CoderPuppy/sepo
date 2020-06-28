{-# LANGUAGE FlexibleContexts #-}

module Sepo.Expr.Runtime where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS (runWriterT, tell)
import Data.Aeson (encodeFile, decodeFileStrict)
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.Foldable (find, for_, toList)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Any(..))
import Data.Traversable (for)
import Prelude hiding (subtract)
import System.IO (stderr, hPutStrLn)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import UnliftIO.Environment (getEnv)
import UnliftIO.IORef
import qualified Control.Monad.Trans.Writer as WriterT
import qualified DBus.Client as DBus (Client, connectSession)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Sepo.Expr.AST
import Sepo.Runtime.Values
import qualified Sepo.DBusClient as DBus
import qualified Sepo.Expr.Parser as Parser
import qualified Sepo.Runtime.Filter as Filter
import qualified Sepo.Runtime.Query as Query
import qualified Sepo.WebClient as HTTP

data Context = Context {
	queryCtx :: Query.Ctx,
	dbusClient :: DBus.Client,
	aliasesPath :: FilePath,
	aliases :: IORef (M.Map (Either T.Text T.Text) Cmd)
}

data Stack = Stack {
	stAliases :: S.Set T.Text
}

initialStack :: Stack
initialStack = Stack { stAliases = S.empty }

start :: (Query.MonadFraxl Source m, MonadIO m) => Query.Ctx -> m Context
start queryCtx = do
	dbusClient <- liftIO DBus.connectSession
	home <- getEnv "HOME"
	Query.dataFetch SCurrentUser
	Query.dataFetch SCurrentUserPlaylists
	let aliasesPath = home <> "/.config/sepo/aliases"
	aliases <- do
		doesFileExist aliasesPath >>= \case
			True -> do
				txt <- liftIO $ T.readFile aliasesPath
				let
					p = fmap M.fromList $ many $
						(,) <$> f <* Parser.ws <* MP.chunk "=" <* Parser.ws <*> fmap Parser.exprCmd Parser.expr <* Parser.ws
					f
						=   MP.chunk "spotify:playlist:" *> fmap Left (MP.takeWhileP (Just "playlist id") isAlphaNum)
						<|> MP.chunk "_" *> fmap Right Parser.quoted
				case MP.runParser (fmap fst $ WriterT.runWriterT p) aliasesPath txt of
					Left err -> do
						liftIO $ hPutStrLn stderr $ MP.errorBundlePretty err
						newIORef M.empty
					Right v -> newIORef v
			False -> do
				liftIO $ writeFile aliasesPath ""
				newIORef M.empty
	pure $ Context {..}

findPlaylist :: (Query.MonadFraxl Source m, MonadIO m) => Context -> T.Text -> m (Maybe Playlist)
findPlaylist ctx name = do
	pls <- Query.dataFetch SCurrentUserPlaylists
	pure $ find ((== name) . playlistName) pls

executeField :: (Query.MonadFraxl Source m, MonadIO m, MonadFail m) => Context -> Stack -> Field -> m (Value m, m Cmd)
executeField ctx stack f@(PlaylistId pl_id) = do
	playlist <- Query.dataFetch $ SPlaylist pl_id
	pure (
			Value {
				-- TODO: check for a Sepo tag for unordered
				tracks = fmap (Ordered . fmap fst) $ Query.dataFetch $ SPlaylistTracks pl_id,
				existing = Just $ ExPlaylist playlist
			},
			pure $ Field $ FieldAccess f []
		)
executeField ctx stack f@(PlaylistName name) = do
	pl <- findPlaylist ctx name >>= maybe (fail $ "unknown playlist: " <> T.unpack name) pure
	(val, _cmd) <- executeField ctx stack $ PlaylistId $ playlistId pl
	pure (val, pure $ Field $ FieldAccess f [])
executeField ctx stack f@(AliasName name) = do
	alias <- readIORef (aliases ctx) >>= maybe (fail $ "unknown alias: " <> T.unpack name) pure . M.lookup (Right name)
	(val, cmd) <- executeCmd ctx (stack { stAliases = S.insert name (stAliases stack) }) alias
	pure (val, if S.member name (stAliases stack) then cmd else pure $ Field $ FieldAccess f [])
executeField ctx stack Playing =
	fmap (, pure $ Field $ FieldAccess Playing []) $
	fmap (fromMaybe $ Value (pure $ Ordered []) Nothing) $
	runMaybeT $ do
		(context, _) <- MaybeT $ Query.dataFetch SCurrentlyPlaying
		(contextType, contextURI) <- MaybeT $ pure context
		lift $ fmap fst $ case contextType of
			HTTP.CTPlaylist -> executeField ctx stack $ PlaylistId $ (!! 2) $ T.splitOn ":" contextURI
			HTTP.CTAlbum -> executeCmd ctx stack $ AlbumId $ (!! 2) $ T.splitOn ":" contextURI
			HTTP.CTArtist -> executeCmd ctx stack $ ArtistId $ (!! 2) $ T.splitOn ":" contextURI
executeField ctx stack f@(File path) = do
	txt <- liftIO $ T.readFile path
	case MP.runParser (WriterT.runWriterT $ Parser.compoundInner <* Parser.ws <* MP.eof) path txt of
		Left err -> fail $ MP.errorBundlePretty err
		Right (cmds, comments) -> do
			cmd <- Parser.handleMode (cmds, comments)
			(val, _cmd) <- executeCmd ctx stack cmd
			pure (val, pure $ Field $ FieldAccess f [])

executeFieldAssignment :: (Query.MonadFraxl Source m, MonadIO m, MonadFail m) => Context -> Stack -> Field -> Cmd -> m (Value m, m Cmd)
executeFieldAssignment ctx stack (PlaylistId pl_id) cmd = do
	(val, cmd') <- executeCmd ctx stack cmd
	tracks <- tracks val
	let chunks = chunksOf 100 $ tracksList tracks
	playlist <- Query.apply (queryCtx ctx) $ APlaylistReplace pl_id $ fromMaybe [] $ listToMaybe chunks
	let addTracks chunk = Query.apply (queryCtx ctx) $ APlaylistAdd pl_id Nothing chunk
	playlist <- foldl ((. addTracks) . (*>)) (pure playlist) (drop 1 chunks)
	liftIO $ T.appendFile (aliasesPath ctx) $ "spotify:playlist:" <> pl_id <> " = " <> reify minBound cmd <> ";\n"
	modifyIORef (aliases ctx) $ M.insert (Left pl_id) cmd
	pure $ (, cmd') $ Value {
		tracks = pure tracks,
		existing = Just $ ExPlaylist playlist
	}
executeFieldAssignment ctx stack (PlaylistName name) cmd = do
	pl_id <- findPlaylist ctx name >>= \case
		Just pl -> pure $ playlistId pl
		Nothing -> fmap playlistId $
			Query.apply (queryCtx ctx) $ APlaylistCreate $
				HTTP.CreatePlaylist name Nothing Nothing (Just "created by Sepo")
	executeFieldAssignment ctx stack (PlaylistId pl_id) cmd
executeFieldAssignment ctx stack (AliasName name) cmd = do
	(val, cmd') <- executeCmd ctx (stack { stAliases = S.insert name (stAliases stack) }) cmd
	cmd' <- cmd'
	liftIO $ T.appendFile (aliasesPath ctx) $ "_" <> reifyQuoted name <> " = " <> reify minBound cmd' <> ";\n"
	modifyIORef (aliases ctx) $ M.insert (Right name) cmd'
	pure (val, pure cmd')
executeFieldAssignment ctx stack Playing cmd = do
	(val, cmd') <- executeCmd ctx stack cmd
	case existing val of
		Just (ExPlaylist pl) -> DBus.play (dbusClient ctx) ("spotify:playlist:" <> playlistId pl)
		Just (ExAlbum al) -> DBus.play (dbusClient ctx) ("spotify:album:" <> albumId al)
		Just (ExArtist ar) -> DBus.play (dbusClient ctx) ("spotify:artist:" <> artistId ar)
		Nothing -> fail "TODO: play"
	pure (val, cmd')
executeFieldAssignment ctx stack (File path) cmd = do
	(val, cmd') <- executeCmd ctx stack cmd
	cmd' <- cmd'
	liftIO $ T.writeFile path $ reify minBound cmd'
	pure (val, pure cmd')

executeCmd :: (Query.MonadFraxl Source m, MonadIO m, MonadFail m) => Context -> Stack -> Cmd -> m (Value m, m Cmd)
executeCmd ctx stack (Field (FieldAccess field [])) = executeField ctx stack field
executeCmd ctx stack (Field (FieldAccess field [cmd])) = do
	(val, cmd') <- executeFieldAssignment ctx stack field cmd
	pure (val, fmap (Field . FieldAccess field . pure) cmd')
executeCmd ctx stack (Field (FieldAccess field (cmd:cmds))) = do
	(_, cmd1) <- executeFieldAssignment ctx stack field cmd
	(val, cmd2) <- executeCmd ctx stack $ Field $ FieldAccess field cmds
	pure (val, Seq <$> fmap (Field . FieldAccess field . pure) cmd1 <*> cmd2)
executeCmd ctx stack (TrackId tr_id) = pure (
		Value {
			tracks = fmap (Ordered . pure) $ Query.dataFetch $ STrack tr_id,
			existing = Nothing
		},
		pure $ TrackId tr_id
	)
executeCmd ctx stack (AlbumId al_id) = do
	album <- Query.dataFetch $ SAlbum al_id
	pure (
			Value {
				tracks = fmap Ordered $ Query.dataFetch $ SAlbumTracks al_id,
				existing = Just $ ExAlbum album
			},
			pure $ AlbumId al_id
		)
executeCmd ctx stack (ArtistId ar_id) = do
	artist <- Query.dataFetch $ SArtist ar_id
	pure (
			Value {
				tracks = do
					albums <- Query.dataFetch $ SArtistAlbums ar_id
					trackss <- for albums $ Query.dataFetch . SAlbumTracks . albumId
					pure $ Unordered $ M.fromList $ fmap (, 1) $ filter (any ((== ar_id) . artistId) . trackArtists) $ join $ trackss
					,
				existing = Just $ ExArtist artist
			},
			pure $ ArtistId ar_id
		)
executeCmd ctx stack PlayingSong =
	fmap (, pure PlayingSong) $
	fmap (fromMaybe $ Value (pure $ Ordered []) Nothing) $
	runMaybeT $ do
		(_, track) <- MaybeT $ Query.dataFetch SCurrentlyPlaying
		pure $ Value {
				tracks = pure $ Ordered [track],
				existing = Nothing
			}
executeCmd ctx stack Empty = pure (vEmpty, pure Empty)
executeCmd ctx stack (Seq a b) = do
	(_, a') <- executeCmd ctx stack a
	(val, b') <- executeCmd ctx stack b
	pure (val, Seq <$> a' <*> b')
executeCmd ctx stack (RevSeq a b) = do
	(val, a') <- executeCmd ctx stack a
	(_, b') <- executeCmd ctx stack b
	pure (val, RevSeq <$> a' <*> b')
executeCmd ctx stack (Concat a b) = do
	(a, a') <- executeCmd ctx stack a
	(b, b') <- executeCmd ctx stack b
	pure (vConcat a b, Concat <$> a' <*> b')
executeCmd ctx stack (Intersect a b) = do
	(a, a') <- executeCmd ctx stack a
	((b, _), b') <- compileFilter ctx stack b
	pure (Filter.vIntersect a b, Intersect <$> a' <*> b')
executeCmd ctx stack (Subtract a b) = do
	(a, a') <- executeCmd ctx stack a
	((b, _), b') <- compileFilter ctx stack b
	pure (Filter.vSubtract a b, Subtract <$> a' <*> b')
executeCmd ctx stack (Unique cmd) = do
	(val, cmd') <- executeCmd ctx stack cmd
	pure (vUnique val, fmap Unique cmd')
executeCmd ctx stack (Shuffle a) = fail "TODO: shuffle"
executeCmd ctx stack (Expand cmd) = do
	(val, cmd') <- executeCmd ctx stack cmd
	pure (val, fmap (foldl Concat Empty . fmap (TrackId . trackId) . tracksList) $ tracks val)
executeCmd ctx stack (SortTrack cmd) = do
	(val, cmd') <- executeCmd ctx stack cmd
	pure (vSortTrack val, fmap SortTrack cmd')
executeCmd ctx stack (SortAlbum cmd) = do
	(val, cmd') <- executeCmd ctx stack cmd
	pure (vSortAlbum val, fmap SortAlbum cmd')
executeCmd ctx stack (SortArtist cmd) = do
	(val, cmd') <- executeCmd ctx stack cmd
	pure (vSortArtist val, fmap SortArtist cmd')

compileFilter :: (Query.MonadFraxl Source m, MonadIO m, MonadFail m) => Context -> Stack -> Cmd -> m ((Filter.Filter, m (Value m)), m Cmd)
compileFilter ctx stack (Field (FieldAccess f@(AliasName name) [])) = do
	alias <- readIORef (aliases ctx) >>= maybe (fail $ "unknown alias: " <> T.unpack name) pure . M.lookup (Right name)
	(res, cmd) <- compileFilter ctx (stack { stAliases = S.insert name (stAliases stack) }) alias
	pure (res, if S.member name (stAliases stack) then cmd else pure $ Field $ FieldAccess f [])
compileFilter ctx stack (TrackId tr_id) = pure (
		(
			mempty { Filter.posPred = \track -> (== tr_id) $ trackId track },
			fmap fst $ executeCmd ctx stack $ TrackId tr_id
		),
		pure $ TrackId tr_id
	)
compileFilter ctx stack (AlbumId al_id) = pure (
		(
			mempty { Filter.posPred = \track -> (== al_id) $ albumId $ trackAlbum track },
			fmap fst $ executeCmd ctx stack $ AlbumId al_id
		),
		pure $ AlbumId al_id
	)
compileFilter ctx stack (ArtistId ar_id) = pure (
		(
			mempty { Filter.posPred = \track -> elem ar_id $ fmap artistId $ trackArtists track },
			fmap fst $ executeCmd ctx stack $ ArtistId ar_id
		),
		pure $ ArtistId ar_id
	)
compileFilter ctx stack Empty = pure ((mempty, pure vEmpty), pure Empty)
compileFilter ctx stack (Seq a b) = do
	(_, a') <- executeCmd ctx stack a
	(b, b') <- compileFilter ctx stack b
	pure (b, Seq <$> a' <*> b')
compileFilter ctx stack (RevSeq a b) = do
	(a, a') <- compileFilter ctx stack a
	(_, b') <- executeCmd ctx stack b
	pure (a, RevSeq <$> a' <*> b')
compileFilter ctx stack (Concat a b) = do
	((af, av), a') <- compileFilter ctx stack a
	((bf, bv), b') <- compileFilter ctx stack b
	pure ((Filter.union af bf, vConcat <$> av <*> bv), Concat <$> a' <*> b')
compileFilter ctx stack (Intersect a b) = do
	((af, av), a') <- compileFilter ctx stack a
	((bf, bv), b') <- compileFilter ctx stack b
	pure ((Filter.union af bf, fmap (flip Filter.vIntersect bf) av), Intersect <$> a' <*> b')
compileFilter ctx stack (Subtract a b) = do
	((af, av), a') <- compileFilter ctx stack a
	((bf, bv), b') <- compileFilter ctx stack b
	pure ((Filter.union af bf, fmap (flip Filter.vSubtract bf) av), Subtract <$> a' <*> b')
compileFilter ctx stack (Unique cmd) = do
	((filter, val), cmd') <- compileFilter ctx stack cmd
	pure ((filter, fmap vUnique val), fmap Unique cmd')
compileFilter ctx stack (Shuffle cmd) = do
	((filter, val), cmd') <- compileFilter ctx stack cmd
	pure ((filter, fail "TODO: shuffle"), fmap Shuffle cmd')
compileFilter ctx stack (Expand cmd) = do
	((filter, val), cmd') <- compileFilter ctx stack cmd
	pure $ ((filter, val),) $ do
		val <- val
		tracks <- tracks val
		pure $ foldl Concat Empty $ fmap (TrackId . trackId) $ tracksList tracks
compileFilter ctx stack (SortTrack cmd) = do
	((filter, val), cmd') <- compileFilter ctx stack cmd
	pure ((filter, fmap vSortTrack val), fmap SortTrack cmd')
compileFilter ctx stack (SortAlbum cmd) = do
	((filter, val), cmd') <- compileFilter ctx stack cmd
	pure ((filter, fmap vSortAlbum val), fmap SortAlbum cmd')
compileFilter ctx stack (SortArtist cmd) = do
	((filter, val), cmd') <- compileFilter ctx stack cmd
	pure ((filter, fmap vSortArtist val), fmap SortArtist cmd')
compileFilter ctx stack cmd = do
	(val, cmd') <- executeCmd ctx stack cmd
	tracks <- tracks val
	pure ((mempty { Filter.posSet = tracksSet tracks }, pure val), cmd')
