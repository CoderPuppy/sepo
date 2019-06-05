module Sepo.Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Void
import Sepo.AST
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import qualified Data.Text as T

aliasPrefixes :: [T.Text]
aliasPrefixes = ["_"]

playlistIdPrefixes :: [T.Text]
playlistIdPrefixes = ["p:", "pl:", "playlist:", "spotify:playlist:"]

playingNames :: [T.Text]
playingNames = ["playing", "current"]

trackIdPrefixes :: [T.Text]
trackIdPrefixes = ["t:", "tr:", "track:", "spotify:track:"]

albumIdPrefixes :: [T.Text]
albumIdPrefixes = ["al:", "album:", "spotify:album:"]

artistIdPrefixes :: [T.Text]
artistIdPrefixes = ["ar:", "artist:", "spotify:artist:"]

playingSongNames :: [T.Text]
playingSongNames = ["playing_song", "current_song"]

emptyNames :: [T.Text]
emptyNames = ["empty", "ε"]

seqOps :: [T.Text]
seqOps = [";"]

assignOps :: [T.Text]
assignOps = ["=", "<-", "←"]

assignConcatOps :: [T.Text]
assignConcatOps = ["+=", "++="]

assignSubtractOps :: [T.Text]
assignSubtractOps = ["-=", "--=", "\\="]

assignIntersectOps :: [T.Text]
assignIntersectOps = ["&=", "&&=", "∩=", "∧="]

concatOps :: [T.Text]
concatOps = ["++", "+", "||", "|", "∪", "∨"]

subtractOps :: [T.Text]
subtractOps = ["--", "-", "\\"]

intersectOps :: [T.Text]
intersectOps = ["&", "&&", "∩", "∧"]

uniqueNames :: [T.Text]
uniqueNames = ["unique", "uniq", "u"]

shuffleNames :: [T.Text]
shuffleNames = ["shuffle", "shuf", "s"]

fieldAssignOps :: [T.Text]
fieldAssignOps = ["!=", "!"]

fieldAssignConcatOps :: [T.Text]
fieldAssignConcatOps = fmap (mappend "!") (concatOps ++ assignConcatOps)

fieldAssignSubtractOps :: [T.Text]
fieldAssignSubtractOps = fmap (mappend "!") (subtractOps ++ assignSubtractOps)

fieldAssignIntersectOps :: [T.Text]
fieldAssignIntersectOps = fmap (mappend "!") (intersectOps ++ assignIntersectOps)

fieldAssignUniqueNames :: [T.Text]
fieldAssignUniqueNames = fmap (mappend "!") uniqueNames

fieldAssignShuffleNames :: [T.Text]
fieldAssignShuffleNames = fmap (mappend "!") shuffleNames

names :: [T.Text]
names = join [
		playingNames,
		playingSongNames,
		emptyNames,
		uniqueNames,
		shuffleNames
	]

prefixes :: [T.Text]
prefixes = join [
		aliasPrefixes,
		playlistIdPrefixes,
		trackIdPrefixes,
		albumIdPrefixes,
		artistIdPrefixes
	]

type Parser = Parsec Void T.Text

ws :: Parser ()
ws = void $ many spaceChar

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

word :: Parser a -> Parser a
word p = try $ p <* notFollowedBy (satisfy isWordChar)

operator :: Parser a -> Parser a
operator p = try $ p <* lookAhead (eof <|> void spaceChar <|> void (satisfy isWordChar))

options :: [T.Text] -> Parser ()
options = asum . fmap (void . chunk)

optionWs :: [T.Text] -> Parser ()
optionWs = asum . fmap (void . word . chunk)

optionOs :: [T.Text] -> Parser ()
optionOs = asum . fmap (void . operator . chunk)

quotedEscape :: Parser Char
quotedEscape = single '\\' *> (
		single 'n' *> pure '\n' <|>
		single 'r' *> pure '\r' <|>
		single 't' *> pure '\t' <|>
		single '\'' <|> single '"'
	)

quotedInner :: Parser () -> Parser T.Text
quotedInner stop = fmap T.pack $ many $ notFollowedBy stop *> (noneOf ['\\'] <|> quotedEscape)

quoted :: Parser T.Text
quoted
	=   single '\'' *> quotedInner (void $ single '\'') <* single '\''
	<|> single '"'  *> quotedInner (void $ single '"' ) <* single '"'

identifier :: Parser T.Text
identifier = notFollowedBy (optionWs names) *> notFollowedBy (options prefixes) *> takeWhile1P (Just "identifier") isWordChar <* notFollowedBy (satisfy isWordChar) <|> quoted

field1 :: Parser FieldAccess
field1
	=   options playlistIdPrefixes *> fmap (flip FieldAccess [] . PlaylistId) (takeWhileP (Just "playlist id") isAlphaNum)
	<|> try (chunk "spotify:user:" *> takeWhileP (Just "spotify username") ((&&) <$> not . isSpace <*> (/= ':')) *> ":playlist:") *> fmap (flip FieldAccess [] . PlaylistId) (takeWhileP (Just "playlist id") isAlphaNum)
	<|> optionWs playingNames *> pure (FieldAccess Playing [])
	<|> options aliasPrefixes *> fmap (flip FieldAccess [] . AliasName) (takeWhile1P (Just "alias") isWordChar <|> quoted)
	<|> fmap (flip FieldAccess [] . PlaylistName) identifier
	<|> try (single '(' *> ws *> field <* ws <* single ')')

field2 :: Parser FieldAccess
field2 = liftA2 (foldl (flip ($))) (field1 <* ws) (many $ asum (fmap try [
		optionOs fieldAssignOps *> ws *> fmap (flip assign) cmd1,
		optionOs fieldAssignConcatOps *> ws *> fmap (flip assConcat) cmd1,
		optionOs fieldAssignSubtractOps *> ws *> fmap (flip assSubtract) cmd1,
		optionOs fieldAssignIntersectOps *> ws *> fmap (flip assIntersect) cmd1,
		optionWs fieldAssignUniqueNames *> pure assUnique,
		optionWs fieldAssignShuffleNames *> pure assShuffle
	]) <* ws)

field :: Parser FieldAccess
field = field2

compound :: Parser [(Bool, Cmd)]
compound = do
	single '{' *> ws
	h <- many $ try $ part <* ws <* single ',' <* ws
	t <- optional $ part <* ws
	single '}'
	pure $ h ++ maybeToList t
	where
		part :: Parser (Bool, Cmd)
		part = (,) <$> fmap (maybe True (const False)) (optional (single '-' *> ws)) <*> cmd

compoundProcess :: [(Bool, Cmd)] -> Cmd
compoundProcess parts = go parts Empty
	where
		go [] h = h
		go ((True, el):t) h = go t (Concat h el)
		go ((False, el):t) h = go t (Subtract h el)

cmd1 :: Parser Cmd
cmd1
	=   fmap Field field1
	<|> options trackIdPrefixes *> fmap TrackId (takeWhileP (Just "track id") isAlphaNum)
	<|> options albumIdPrefixes *> fmap AlbumId (takeWhileP (Just "album id") isAlphaNum)
	<|> options artistIdPrefixes *> fmap ArtistId (takeWhileP (Just "artist id") isAlphaNum)
	<|> optionWs playingSongNames *> pure PlayingSong
	<|> single '(' *> ws *> cmd <* ws <* single ')'
	<|> fmap compoundProcess compound
	<|> optionWs emptyNames *> pure Empty

cmd2 :: Parser Cmd
cmd2 = fmap Field field2 <|> cmd1

cmd3 :: Parser Cmd
cmd3 = liftA2 (foldl (flip ($))) (cmd2 <* ws) $ many $ fmap flip part <*> (ws *> cmd2 <* ws)
	where 
		part :: Parser (Cmd -> Cmd -> Cmd)
		part = optionOs intersectOps *> pure Intersect

cmd4 :: Parser Cmd
cmd4 = liftA2 (foldl (flip ($))) (cmd3 <* ws) $ many $ fmap flip part <*> (ws *> cmd3 <* ws)
	where
		part :: Parser (Cmd -> Cmd -> Cmd)
		part
			=   optionOs concatOps *> pure Concat
			<|> optionOs subtractOps *> pure Subtract


cmd5 :: Parser Cmd
cmd5 = flip (foldr ($)) <$> many (part <* ws) <*> cmd4
	where
		part :: Parser (Cmd -> Cmd)
		part
			=   optionWs uniqueNames *> pure Unique
			<|> optionWs shuffleNames *> pure Shuffle

			<|> chunk "!" *> pure (Field . assign (FieldAccess Playing []))

			<|> try ((Field .) . assign <$> field <* ws <* optionOs assignOps)
			<|> try ((Field .) . assConcat <$> field <* ws <* optionOs assignConcatOps)
			<|> try ((Field .) . assSubtract <$> field <* ws <* optionOs assignSubtractOps)

cmd6 :: Parser Cmd
cmd6 = foldr Seq <$> (cmd5 <* ws) <*> many (optionOs seqOps *> ws *> cmd5)

cmd :: Parser Cmd
cmd = cmd6
