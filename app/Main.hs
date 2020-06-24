module Main where

import Conduit ((.|))
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Bool (bool)
import Data.Dependent.Sum
import Data.Foldable
import Data.List (intercalate)
import Data.String
import System.Exit (exitFailure)
import System.IO (stderr, hPutStr, hPutStrLn)
import Text.Megaparsec (runParser, eof, errorBundlePretty)
import UnliftIO.Environment (getArgs, getEnv)
import UnliftIO.IORef
import qualified Conduit as Conduit
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
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

data OutputFormat = OFSimple Bool Bool | OFFile Bool | OFJSON deriving (Show)
readOutputFormat :: Args.ReadM OutputFormat
readOutputFormat = Args.maybeReader $ \case
	"simple"  -> Just $ OFSimple True False
	"simple+" -> Just $ OFSimple True True
	"simple-"  -> Just $ OFSimple False False
	"simple-+" -> Just $ OFSimple False True
	"simple+-" -> Just $ OFSimple False True
	"file"  -> Just $ OFFile False
	"file+" -> Just $ OFFile True
	"json" -> Just OFJSON
	_ -> Nothing

data Options = Options {}
data Command
	= CEval EvalOpts
	| CFetch FetchOpts
	deriving (Show)
data EvalOpts = EvalOpts {
	evalFormat :: OutputFormat,
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
			liftIO $ hPutStrLn stderr "Parse error"
			liftIO $ hPutStr stderr $ errorBundlePretty err
			liftIO exitFailure
		Right cmd -> pure cmd
	case evalFormat of
		OFSimple header _ -> when header $ liftIO $ T.putStrLn $ Expr.reify minBound cmd
		OFFile _ -> liftIO $ T.putStrLn $ "# " <> Expr.reify minBound cmd
		OFJSON -> pure ()
	exprCtx <- Expr.start queryCtx
	val <- Expr.executeCmd exprCtx cmd
	tracks <- force $ tracks val
	case evalFormat of
		-- TODO: lifting this block as a whole seems to be necessary to ensure proper order of message
		-- this means something is very broken, probably in Fraxl
		-- also see below
		OFSimple header full -> liftIO $ do
			when header $ do
				putStrLn $ case tracks of
					Ordered _ -> "ordered"
					Unordered _ -> "unordered"
				maybe (pure ()) T.putStrLn $ flip fmap (existing val) $ \case
					ExArtist ar -> "artist " <> artistName ar <> " (spotify:artist:" <> artistId ar <> ")"
					ExAlbum al -> "album " <> albumName al <> " featuring " <> formatList "no-one" (fmap artistName $ albumArtists al) <> " (spotify:album:" <> albumId al <> ")"
					ExPlaylist pl -> "playlist " <> playlistName pl <> " (spotify:playlist:" <> playlistId pl <> ")"
			for_ (tracksList tracks) $ \track -> T.putStrLn $ mconcat [
					trackName track,
					" by ", formatList "no-one" $ fmap artistName $ trackArtists track,
					" from ", albumName $ trackAlbum track,
					" (spotify:track:", trackId track,
					if full
					then mconcat [
							" by ", formatList "no-one" $ fmap (("spotify:artist:" <>) . artistId) $ trackArtists track,
							" from spotify:album:", albumId $ trackAlbum track
						]
					else "",
					")"
				]
		-- TODO: see above
		OFFile full -> liftIO $ do
			putStrLn $ case tracks of
				Ordered _ -> "# ordered"
				Unordered _ -> "# unordered"
			maybe (pure ()) T.putStrLn $ flip fmap (existing val) $ \case
				ExArtist ar -> "# artist " <> artistName ar <> " (spotify:artist:" <> artistId ar <> ")"
				ExAlbum al -> "# album " <> albumName al <> " featuring " <> formatList "no-one" (fmap artistName $ albumArtists al) <> " (spotify:album:" <> albumId al <> ")"
				ExPlaylist pl -> "# playlist " <> playlistName pl <> " (spotify:playlist:" <> playlistId pl <> ")"
			for_ (tracksList tracks) $ \track -> T.putStrLn $ mconcat [
					"spotify:track:", trackId track,
					" # ", trackName track,
					" by ", formatList "no-one" $ fmap artistName $ trackArtists track,
					" from ", albumName $ trackAlbum track,
					if full
					then mconcat [
							" (by ", formatList "no-one" $ fmap (("spotify:artist:" <>) . artistId) $ trackArtists track,
							" from spotify:album:", albumId $ trackAlbum track,
							")"
						]
					else ""
				]
		OFJSON -> liftIO $ BSL.putStrLn $ Aeson.encodingToLazyByteString $ Aeson.pairs $ mconcat [
				Aeson.pair "command" $ Aeson.toEncoding $ Expr.reify minBound cmd,
				Aeson.pair "tracks" $ Aeson.pairs $ mconcat [
					Aeson.pair "ordered" $ Aeson.toEncoding $ case tracks of
						Ordered _ -> True
						Unordered _ -> False
						,
					Aeson.pair "list" $ Aeson.toEncoding $ tracksList tracks,
					Aeson.pair "set" $ encodeTracksSet $ tracksSet tracks
				],
				Aeson.pair "existing" $ Aeson.toEncoding $ existing val
			]
runCmd opts queryCtx (CFetch (FetchOpts {..})) = do
	ops <- pure []
	ops <- fmap (++ ops) $ flip (bool (pure [])) fetchUserPlaylists $ do
		liftIO $ putStrLn "fetching user playlists"
		pure $ pure (
				do
					pls <- Query.dataFetch Query.SCurrentUserPlaylists
					for_ pls $ Query.dataFetch . SPlaylistTracks . playlistId
					,
				pure ()
			)
	ops <- fmap (++ ops) $ flip (bool (pure [])) fetchExisting $ do
		liftIO $ putStrLn "refetching cached"
		useFSCache <- liftIO $ atomicModifyIORef (Query.ctxUseFSCache queryCtx) $ const False &&& id
		entries <- FSCache.entries (Query.ctxCachePath queryCtx)
		pure $ pure (
				for_ entries $ \(src :=> path) -> do
					void $ Query.dataFetch src
					,
				atomicWriteIORef (Query.ctxUseFSCache queryCtx) useFSCache
			)
	for_ ops fst
	for_ ops snd

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
					<$> ((<|> pure (OFSimple True False)) $ Args.option readOutputFormat (Args.long "format" <> Args.metavar "FORMAT" <> Args.help "format to output in, valid options: simple[-][+], file[+], json"))
					<*> (fmap (T.intercalate " ") $ some $ Args.argument Args.str (Args.metavar "EXPR" <> Args.help "expression to evaluate"))
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
