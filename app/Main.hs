module Main where

import Data.Foldable
import Data.String
import Sepo.AST
import Sepo.Runtime.Execution
import Sepo.Runtime.Values
import Sepo.Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (runParser, eof, errorBundlePretty)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Trie.Text as Trie
import System.IO (stderr)

cmds :: Trie.Trie ([T.Text] -> IO ())
cmds = Trie.fromList [
		(("eval",) $ \args -> do
			let txt = T.intercalate " " args
			cmd <- case runParser (cmd <* eof) "cmdline" txt of
				Left err -> do
					putStrLn "Parse error"
					putStr $ errorBundlePretty err
					exitFailure
				Right cmd -> pure cmd
			T.putStrLn $ reify PSeq cmd
			ctx <- start
			val <- executeCmd ctx cmd
			tracks <- force $ tracks val
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
					" (spotify:track:", trackId track, ")"
				]
			)
	]

main :: IO ()
main = do
	args <- getArgs
	ok <- case fmap T.pack args of
		(cmdName:args) -> case Trie.toList $ Trie.submap cmdName cmds of
			[] -> pure $ Left $ "no command matching: " <> TL.fromStrict cmdName
			[(_, cmd)] -> fmap (const $ Right ()) $ cmd args
			cmds -> pure $ Left $ "multiple commands matched: " <> TL.fromStrict cmdName <> ": " <> TL.intercalate ", " (fmap fst cmds)
		[] -> pure $ Left $ "no arguments"
	case ok of
		Left msg -> do
			TL.hPutStrLn stderr msg
			T.hPutStrLn stderr "usage: sepo <command> [<args>]"
			for_ (Trie.toList cmds) $ \(name, _) -> do
				TL.hPutStrLn stderr $ "  sepo " <> name
			exitFailure
		Right () -> pure ()

formatList :: (IsString a, Semigroup a, Foldable f) => a -> f a -> a
formatList empty lst = maybe empty (uncurry $ maybe id ((<>) . (<> ", and "))) acc
	where acc = foldl (\acc el -> Just (fmap (uncurry $ maybe id ((<>) . (<> ", "))) acc, el)) Nothing lst
