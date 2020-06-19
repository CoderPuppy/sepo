module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.String
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Megaparsec (runParser, eof, errorBundlePretty)
import UnliftIO.Environment (getArgs, getEnv)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Args

import Sepo.Runtime.Values
import qualified Sepo.Expr.AST as Expr
import qualified Sepo.Expr.Parser as Expr
import qualified Sepo.Expr.Runtime as Expr
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
	fetchUserPlaylists :: Bool
} deriving (Show)

runCmd :: Options -> Query.Ctx -> Command -> Query.FreerT Query.Source IO ()
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
		))

main :: IO ()
main = do
	home <- getEnv "HOME"
	let cachePath = home <> "/.cache/sepo"
	queryCtx <- Query.start cachePath
	(opts, cmd) <- Args.execParser args
	Query.run queryCtx $ runCmd opts queryCtx cmd

formatList :: (IsString a, Semigroup a, Foldable f) => a -> f a -> a
formatList empty lst = maybe empty (uncurry $ maybe id ((<>) . (<> ", and "))) acc
	where acc = foldl (\acc el -> Just (fmap (uncurry $ maybe id ((<>) . (<> ", "))) acc, el)) Nothing lst
