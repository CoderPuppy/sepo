module Main where

import Control.Monad.IO.Class
import Data.Foldable
import Data.String
import UnliftIO.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Megaparsec (runParser, eof, errorBundlePretty)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Trie.Text as Trie

import qualified Sepo.Expr.AST as Expr
import qualified Sepo.Expr.Runtime as Expr
import Sepo.Runtime.Values
import qualified Sepo.Expr.Parser as Expr
import qualified Sepo.Runtime.Query as Query

cmds :: Query.Ctx -> Trie.Trie ([T.Text] -> Query.FreerT Query.Source IO ())
cmds queryCtx = Trie.fromList [
		(("eval",) $ \args -> do
			let txt = T.intercalate " " args
			cmd <- case runParser (Expr.cmd <* eof) "cmdline" txt of
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
			),
		(("fetch",) $ \args -> do
			pls <- Query.dataFetch Query.SCurrentUserPlaylists
			for_ pls $ Query.dataFetch . SPlaylistTracks . playlistId
			)
	]

main :: IO ()
main = do
	home <- getEnv "HOME"
	let cachePath = home <> "/.cache/sepo"
	queryCtx <- Query.start cachePath
	Query.run queryCtx $ do
		args <- getArgs
		ok <- case fmap T.pack args of
			(cmdName:args) -> case Trie.toList $ Trie.submap cmdName (cmds queryCtx) of
				[] -> pure $ Left $ "no command matching: " <> TL.fromStrict cmdName
				[(_, cmd)] -> fmap (const $ Right ()) $ cmd args
				cmds -> pure $ Left $ "multiple commands matched: " <> TL.fromStrict cmdName <> ": " <> TL.intercalate ", " (fmap fst cmds)
			[] -> pure $ Left $ "no arguments"
		case ok of
			Left msg -> do
				liftIO $ TL.hPutStrLn stderr msg
				liftIO $ T.hPutStrLn stderr "usage: sepo <command> [<args>]"
				for_ (Trie.toList $ cmds queryCtx) $ \(name, _) -> do
					liftIO $ TL.hPutStrLn stderr $ "  sepo " <> name
				liftIO exitFailure
			Right () -> pure ()

formatList :: (IsString a, Semigroup a, Foldable f) => a -> f a -> a
formatList empty lst = maybe empty (uncurry $ maybe id ((<>) . (<> ", and "))) acc
	where acc = foldl (\acc el -> Just (fmap (uncurry $ maybe id ((<>) . (<> ", "))) acc, el)) Nothing lst
