{-# LANGUAGE FlexibleContexts #-}

module Sepo.Expr.Parser where

import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer
import Data.Bool (bool)
import Data.Char
import Data.Foldable
import Data.List (sortOn)
import Data.Maybe
import Data.Void (Void)
import Sepo.Expr.AST
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Trie.Text as Trie

type Parser a = WriterT [State T.Text Void] (Parsec Void T.Text) a

data Expr = EField FieldAccess | ECmd Cmd deriving (Show)
exprField :: MonadFail m => Expr -> m FieldAccess
exprField (EField f) = pure f
exprField (ECmd cmd) = fail $ "expected field, got: " <> T.unpack (reify minBound cmd)
exprCmd :: Expr -> Cmd
exprCmd (EField f) = Field f
exprCmd (ECmd cmd) = cmd

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

wsFlat :: Parser ()
wsFlat = void $ do
	takeWhileP (Just "flat whitespace") $ isSpace
	optional $ do
		single '#'
		state <- getParserState
		text <- takeWhileP (Just "comment") $ (&&) <$> (/= '\n') <*> (/= '\r')
		tell $ pure $ state { stateInput = text }

ws :: Parser ()
ws = do
	wsFlat
	skipMany $ do
		takeWhile1P (Just "newline") $ (||) <$> (== '\n') <*> (== '\r')
		wsFlat

options :: [T.Text] -> Parser ()
options = asum . fmap (void . chunk) . sortOn (negate . T.length)

data PrefixEntry = PrefixEntry {
	preName :: T.Text,
	preExclusive :: Bool,
	preParse :: Parser Expr
}

prefixBuild :: [([T.Text], PrefixEntry)] -> Trie.Trie PrefixEntry
prefixBuild es = 
	foldl
		(\trie (prefix, e) -> let
				conflicting = do
					(prefix', e', _) <- Trie.match trie prefix
					bool Nothing (Just ()) $ preExclusive e'
					pure (prefix', e')
			in case conflicting of
				Just (prefix', e') -> error $
					"conflict between " <> T.unpack (preName e) <> " (at " <> show prefix <> ")" <>
					" and " <> T.unpack (preName e') <> " (at " <> show prefix' <> ")"
				Nothing -> Trie.insert prefix e trie
			)
		Trie.empty $
	sortOn (T.length . fst) $
	do
		(pres, e) <- es
		pre <- pres
		pure (pre, e)

prefixParse :: Trie.Trie PrefixEntry -> Parser Expr
prefixParse table = do
	word <- getInput
	case Trie.match table word of
		Just (prefix, e, _) -> do
			chunk prefix
			label (T.unpack $ preName e) $ preParse e
		Nothing -> empty

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_' || c == ':'

isOperatorChar :: Char -> Bool
isOperatorChar c = not $ isSpace c || isWordChar c

unaryOps :: [([T.Text], Cmd -> Cmd)]
unaryOps = execWriter $ do
	tell $ pure (["unique", "uniq", "u"], Unique)
	tell $ pure (["shuffle", "shuf", "s"], Unique)
	tell $ pure (["expand", "exp", "e"], Expand)
	tell $ pure (["sort_track", "sortTrack", "sort_t", "sort_tr", "str", "st"], SortTrack)
	tell $ pure (["sort_album", "sortAlbum", "sort_al", "sal"], SortAlbum)
	tell $ pure (["sort_artist", "sortArtist", "sort_ar", "sar"], SortArtist)
	tell $ pure (["unordered", "unorder"], Unorder)

assignableOp :: MonadFail m => Bool -> (Cmd -> Cmd) -> Expr -> m Expr
assignableOp modify op e = do
	case modify of
		False -> pure $ ECmd $ op $ exprCmd e
		True -> do
			field <- exprField e
			pure $ EField $ assignOp field op

compoundUnion :: [(Bool, Cmd)] -> Cmd
compoundUnion parts = go parts Empty
	where
		go [] h = h
		go ((True, el):t) h = go t (Concat h el)
		go ((False, el):t) h = go t (Subtract h el)

compoundIntersect :: [(Bool, Cmd)] -> Cmd
compoundIntersect parts = go parts Empty
	where
		go [] h = h
		go ((True, el):t) h = go t (Intersect h el)
		go ((False, el):t) h = go t (Concat h el)

compoundSequence :: [(Bool, Cmd)] -> Cmd
compoundSequence parts = go parts Empty
	where
		go [] h = h
		go ((True, el):t) h = go t (Seq h el)
		go ((False, el):t) h = go t (RevSeq h el)

compoundInner' :: forall a. Parser a -> Parser [(Bool, a)]
compoundInner' obj = do
	h <- many $ try $ try (ws *> part) <* optional (try $ ws *> oneOf [',', ';'])
	t <- optional $ try $ ws *> part
	pure $ h ++ maybeToList t
	where
		part :: Parser (Bool, a)
		part = (,) <$> fmap (maybe True (const False)) (optional (single '-' *> ws)) <*> obj

compoundInner :: Parser [(Bool, Cmd)]
compoundInner = compoundInner' $ fmap exprCmd expr

compound :: Parser [(Bool, Cmd)]
compound = single '{' *> compoundInner <* ws <* single '}'

postops :: Parser Expr -> (Expr -> Parser (Parser Expr)) -> Parser Expr
postops base post = base >>= go
	where go left = ap <|> pure left
		where ap = do
			op <- try $ ws *> post left
			op <- op
			go op

binopsl :: Parser Expr -> Parser (Expr -> Expr -> Parser Expr) -> Parser Expr
binopsl operand operator = postops operand $ \left -> do
	op <- operator
	pure $ do
		ws
		right <- operand
		op left right

binopsr :: Parser Expr -> Parser (Expr -> Expr -> Parser Expr) -> Parser Expr
binopsr operand operator = operand >>= go []
	where go ops last = ap <|> done
		where
			ap = do
				op <- try $ ws *> operator
				ws
				right <- operand
				go ((last, op):ops) right
			done = foldlM (\right (left, op) -> op left right) last ops

binop :: [T.Text] -> (Char -> Bool) -> (Cmd -> Cmd -> Cmd) -> Parser (Expr -> Expr -> Parser Expr)
binop names isEndChar op = do
	modify <- optional $ single '!'
	options names
	notFollowedBy $ satisfy isEndChar
	pure $ flip $ assignableOp (isJust modify) . flip op . exprCmd

prefixes :: Trie.Trie PrefixEntry
prefixes = prefixBuild $ execWriter $ do
	tell $ pure $ (["("],) $ PrefixEntry "parentheses" True $ expr <* single ')'
	tell $ pure $ (["_"],) $ PrefixEntry "alias" True $
		fmap (EField . flip FieldAccess [] . AliasName) $
		quoted <|> takeWhile1P (Just "alias name") isWordChar
	tell $ pure $ ([""],) $ PrefixEntry "playlist name" False $
		fmap (EField . flip FieldAccess [] . PlaylistName) $
		quoted <|> takeWhile1P (Just "playlist name") isWordChar
	tell $ pure $ (["p:", "pl:", "playlist:", "spotify:playlist:"],) $ PrefixEntry "playlist id" True $ do
		pl_id <- takeWhile1P (Just "playlist id") isAlphaNum
		notFollowedBy $ satisfy isWordChar
		pure $ EField $ flip FieldAccess [] $ PlaylistId pl_id
	tell $ pure $ (["spotify:user:"],) $ PrefixEntry "user playlist id" True $ do
		takeWhile1P (Just "user id") isAlphaNum
		chunk ":playlist:"
		pl_id <- takeWhile1P (Just "playlist id") isAlphaNum
		notFollowedBy $ satisfy isWordChar
		pure $ EField $ flip FieldAccess [] $ PlaylistId pl_id
	tell $ pure $ (["t:", "tr:", "track:", "spotify:track:"],) $ PrefixEntry "track id" True $ do
		tr_id <- takeWhile1P (Just "track id") isAlphaNum
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd $ TrackId tr_id
	tell $ pure $ (["al:", "album:", "spotify:album:"],) $ PrefixEntry "album id" True $ do
		al_id <- takeWhile1P (Just "album id") isAlphaNum
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd $ AlbumId al_id
	tell $ pure $ (["ar:", "artist:", "spotify:artist:"],) $ PrefixEntry "artist id" True $ do
		ar_id <- takeWhile1P (Just "artist id") isAlphaNum
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd $ ArtistId ar_id
	tell $ pure $ (["playing", "current", "this"],) $ PrefixEntry "playing" False $ do
		notFollowedBy $ satisfy isWordChar
		pure $ EField $ FieldAccess Playing []
	tell $ pure $ (((<>) <$> ["playing_", "current_", "this_"] <*> ["song", "track"]),) $ PrefixEntry "playing song" False $ do
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd PlayingSong
	tell $ pure $ (["my_playlists", "playlists"],) $ PrefixEntry "my playlists" False $ do
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd MyPlaylists
	tell $ pure $ (["my_artists", "artists"],) $ PrefixEntry "my artists" False $ do
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd MyArtists
	tell $ pure $ (["my_albums", "albums"],) $ PrefixEntry "my albums" False $ do
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd MyAlbums
	tell $ pure $ (["my_tracks", "tracks"],) $ PrefixEntry "my tracks" False $ do
		notFollowedBy $ satisfy isWordChar
		pure $ EField $ FieldAccess MyTracks []
	tell $ pure $ (["empty", "ε", "ø"],) $ PrefixEntry "empty" False $ do
		notFollowedBy $ satisfy isWordChar
		pure $ ECmd Empty
	tell $ pure $ (["union"],) $ PrefixEntry "union" False $ do
		notFollowedBy $ satisfy isWordChar
		fmap (ECmd . compoundUnion) $ ws *> compound
	tell $ pure $ (["intersect", "intersection"],) $ PrefixEntry "intersect" False $ do
		notFollowedBy $ satisfy isWordChar
		fmap (ECmd . compoundIntersect) $ ws *> compound
	tell $ pure $ (["seq", "sequence"],) $ PrefixEntry "sequence" False $ do
		notFollowedBy $ satisfy isWordChar
		fmap (ECmd . compoundSequence) $ ws *> compound
	do
		let isPathEndChar c = isSpace c || c == ')'
		let doPath prefix = do
			parts <- many $ takeWhile1P (Just "path") (\c -> not $ isPathEndChar c || c == '"' || c == '\'') <|> quoted
			notFollowedBy $ satisfy $ \c -> not $ isPathEndChar c
			pure $ EField $ flip FieldAccess [] $ File $ mconcat $ fmap T.unpack (prefix:parts)
		tell $ pure $ (["./"],) $ PrefixEntry "relative path" True $ doPath "./"
		tell $ pure $ (["/"],) $ PrefixEntry "relative path" True $ doPath "/"

	for_ unaryOps $ \(names, op) -> do
		tell $ pure $ (names,) $ PrefixEntry (head names) False $ do
			notFollowedBy $ satisfy isWordChar
			modify <- optional $ single '!' *> notFollowedBy (satisfy isOperatorChar)
			ws
			e <- exprPrecs !! 5
			assignableOp (isJust modify) op e

exprPrecs = scanl (flip ($))
	(prefixParse prefixes)
	[
		\next -> postops next $ \left -> asum $ flip fmap unaryOps $ \(names, op) -> do
			single '!'
			options names
			notFollowedBy $ satisfy isWordChar
			pure $ do
				field <- exprField left
				pure $ EField $ assignOp field op
			,
		\next -> binopsl next $ binop ["&&", "&", "∩", "∧"] isOperatorChar Intersect,
		\next -> binopsl next
			$   binop ["++", "+", "||", "|", "∪", "∨"] isOperatorChar Concat
			<|> binop ["--", "-", "\\\\", "\\"] isOperatorChar Subtract
			,
		\next -> binopsl next
			$   binop ["*>"] isOperatorChar Seq
			<|> binop ["<*"] isOperatorChar RevSeq
			,
		let
			op names op = do
				options names
				single '='
				notFollowedBy $ satisfy isOperatorChar
				pure $ flip $ assignableOp True . flip op . exprCmd
			opAssign = do
				single '='
				notFollowedBy $ satisfy isOperatorChar
				pure $ \left right -> do
					left <- exprField left
					pure $ EField $ assign left $ exprCmd right
		in \next -> binopsr next
			$   op ["++", "+", "||", "|", "∪", "∨"] Concat
			<|> op ["--", "-", "\\\\", "\\"] Subtract
			<|> opAssign
	]

expr :: Parser Expr
expr = head $ reverse exprPrecs

findMode :: MonadFail m => [T.Text] -> m ([(Bool, Cmd)] -> Cmd)
findMode comments = case comments >>= (toList . T.stripPrefix "SEPO:MODE ") of
	[] -> pure compoundUnion
	["seq"] -> pure compoundSequence
	["sequence"] -> pure compoundSequence
	["union"] -> pure compoundUnion
	["intersect"] -> pure compoundIntersect
	modes -> fail $ "too many and/or invalid modes (seq | union | intersect): " <> show modes

handleMode :: MonadFail m => ([(Bool, Cmd)], [State T.Text Void]) -> m Cmd
handleMode (cmds, comments) = fmap ($ cmds) $ findMode $ fmap stateInput comments
