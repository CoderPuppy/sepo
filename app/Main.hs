module Main where

import Data.Foldable
import Data.String
import Sepo.AST
import Sepo.Execution
import Sepo.Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (runParser, eof, errorBundlePretty)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
	args <- getArgs
	let txt = T.intercalate " " $ fmap T.pack args
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

formatList :: (IsString a, Semigroup a, Foldable f) => a -> f a -> a
formatList empty lst = maybe empty (uncurry $ maybe id ((<>) . (<> ", and "))) acc
	where acc = foldl (\acc el -> Just (fmap (uncurry $ maybe id ((<>) . (<> ", "))) acc, el)) Nothing lst
