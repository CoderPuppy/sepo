module Main where

import Conduit ((.|))
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Dependent.Sum
import Data.Foldable
import Data.List (intercalate)
import Data.String
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Megaparsec (runParser, eof, errorBundlePretty)
import UnliftIO.Environment (getArgs, getEnv)
import UnliftIO.IORef
import qualified Conduit as Conduit
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Args

import Sepo.Runtime.Values
import qualified Sepo.Expr.AST as Expr
import qualified Sepo.Expr.Parser as Expr
import qualified Sepo.Expr.Runtime as Expr
import qualified Sepo.Runtime.FSCache as FSCache
import qualified Sepo.Runtime.Query as Query

data Options = Options {}
data Command
	= CEval EvalOpts
	| CFetch FetchOpts
	deriving (Show)
data EvalOpts = EvalOpts {
	evalTxt :: T.Text
} deriving (Show)
data FetchOpts = FetchOpts {
	fetchUserPlaylists :: Bool,
	fetchExisting :: Bool
} deriving (Show)

type M = Query.FreerT Query.Source (Conduit.ResourceT IO)

runCmd :: Options -> Query.Ctx -> Command -> M ()
runCmd opts queryCtx (CEval (EvalOpts {..})) = do
	cmd <- case runParser (fmap fst $ runWriterT $ fmap Expr.exprCmd Expr.expr <* eof) "cmdline" evalTxt of
		Left err -> do
			liftIO $ putStrLn "Parse error"
			liftIO $ putStr $ errorBundlePretty err
			liftIO exitFailure
		Right cmd -> pure cmd
	liftIO $ T.putStrLn $ Expr.reify Expr.PSeq cmd
	exprCtx <- Expr.start $ Query.ctxHTTP queryCtx
	val <- Expr.executeCmd exprCtx cmd
	tracks <- force $ tracks val
	liftIO $ putStrLn $ case tracks of
		Ordered _ -> "ordered"
		Unordered _ -> "unordered"
	maybe (pure ()) (liftIO . T.putStrLn) $ flip fmap (existing val) $ \case
		ExArtist ar -> "artist " <> artistName ar <> " (spotify:artist:" <> artistId ar <> ")"
		ExAlbum al -> "album " <> albumName al <> " featuring " <> formatList "no-one" (fmap artistName $ albumArtists al) <> " (spotify:album:" <> albumId al <> ")"
		ExPlaylist pl -> "playlist " <> playlistName pl <> " (spotify:playlist:" <> playlistId pl <> ")"
	for_ (tracksList tracks) $ \track -> liftIO $ T.putStrLn $ mconcat [
			trackName track,
			" by ", formatList "no-one" $ fmap artistName $ trackArtists track,
			" from ", albumName $ trackAlbum track,
			" (spotify:track:", trackId track,
			" by ", formatList "no-one" $ fmap (("spotify:artist:" <>) . artistId) $ trackArtists track,
			" from spotify:album:", albumId $ trackAlbum track,
			")"
		]
runCmd opts queryCtx (CFetch (FetchOpts {..})) = do
	when fetchUserPlaylists $ do
		liftIO $ putStrLn "fetching user playlists"
		pls <- Query.dataFetch Query.SCurrentUserPlaylists
		for_ pls $ Query.dataFetch . SPlaylistTracks . playlistId
	when fetchExisting $ do
		liftIO $ putStrLn "refetching cached"
		useFSCache <- liftIO $ atomicModifyIORef (Query.ctxUseFSCache queryCtx) $ const False &&& id
		-- Conduit.runConduit $
		-- 	(FSCache.entries (Query.ctxCachePath queryCtx) .|) $
		-- 	Conduit.mapM_C $ \(src :=> path) -> do
		-- 		void $ Query.dataFetch src
		entries <- FSCache.entries (Query.ctxCachePath queryCtx)
		-- liftIO $ putStrLn $ intercalate ", " $ fmap (\(src :=> path) -> show (src, path)) $ entries
		for_ entries $ \(src :=> path) -> do
			void $ Query.dataFetch src
		atomicWriteIORef (Query.ctxUseFSCache queryCtx) useFSCache

args :: Args.ParserInfo (Options, Command)
args = flip Args.info
	(  Args.fullDesc
	<> Args.header "sepo - a Spotify helper tool")
	((Args.helper <*>) $ liftA2 (,)
		(pure Options)
		(Args.hsubparser $ execWriter $ do
			tell $ Args.command "eval" $ flip Args.info
				mempty
				$ fmap CEval $ EvalOpts
					<$> (fmap (T.intercalate " ") $ some $ Args.argument Args.str (Args.metavar "EXPR" <> Args.help "expression to evaluate"))
			tell $ Args.command "fetch" $ flip Args.info
				mempty
				$ fmap CFetch $ FetchOpts
					<$> (fmap (foldl (const id) True) $ many
							$   Args.flag' False (Args.long "no-user-playlists" <> Args.help "do not fetch the user's playlists")
							<|> Args.flag' True (Args.long "user-playlists"))
					<*> (fmap (foldl (const id) False) $ many
							$   Args.flag' False (Args.long "no-existing")
							<|> Args.flag' True (Args.long "existing" <> Args.help "refetch everything existing in the cache"))
		))

main :: IO ()
main = do
	home <- getEnv "HOME"
	queryCtx <- Query.start $ home <> "/.cache/sepo"
	Conduit.runResourceT $ Query.run queryCtx $ do
		(opts, cmd) <- liftIO $ Args.execParser args
		runCmd opts queryCtx cmd

formatList :: (IsString a, Semigroup a, Foldable f) => a -> f a -> a
formatList empty lst = maybe empty (uncurry $ maybe id ((<>) . (<> ", and "))) acc
	where acc = foldl (\acc el -> Just (fmap (uncurry $ maybe id ((<>) . (<> ", "))) acc, el)) Nothing lst
