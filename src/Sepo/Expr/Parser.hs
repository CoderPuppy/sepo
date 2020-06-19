{-# LANGUAGE FlexibleContexts #-}

module Sepo.Expr.Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer
import Data.Char
import Data.Foldable
import Data.List (sortOn)
import Data.Maybe
import Data.Void
import Sepo.Expr.AST
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Trie.Text as Trie

type Parser a = WriterT [State T.Text Void] (Parsec Void T.Text) a

data Expr = EField FieldAccess | ECmd Cmd deriving (Show)
exprField :: MonadFail m => Expr -> m FieldAccess
exprField (EField f) = pure f
exprField (ECmd _) = fail "expected field"
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
options = asum . fmap (void . chunk)

data PrefixMode = Exact | MostPrecise | Always deriving (Show, Eq)

data PrefixEntry = PrefixEntry {
	preName :: T.Text,
	preMode :: PrefixMode,
	preParse :: T.Text -> Parser Expr
}

prefixBuild :: [([T.Text], PrefixEntry)] -> Trie.Trie PrefixEntry
prefixBuild es = 
	foldl
		(\trie (prefix, e) -> let
				conflicting = do
					(prefix', e', _) <- Trie.match trie prefix
					case preMode e' of
						Always -> Just ()
						_ -> Nothing
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

prefixParse :: Trie.Trie PrefixEntry -> Parser a -> Parser Expr
prefixParse table word = do
	(word, _) <- lookAhead $ match word
	case Trie.match table word of
		Just (prefix, e, rest) -> case preMode e of
			Exact | rest /= "" -> empty
			_ -> do
				chunk prefix
				preParse e rest
		Nothing -> empty

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_' || c == ':'

isOperatorChar :: Char -> Bool
isOperatorChar c = not $ isSpace c || isWordChar c

unaryOps :: [([T.Text], Cmd -> Cmd)]
unaryOps = execWriter $ do
	tell $ pure (["unique", "uniq", "u"], Unique)
	tell $ pure (["shuffle", "shuf", "s"], Unique)

assignableOp :: MonadFail m => Bool -> (Cmd -> Cmd) -> Expr -> m Expr
assignableOp modify op e = do
	case modify of
		False -> pure $ ECmd $ op $ exprCmd e
		True -> do
			field <- exprField e
			pure $ EField $ assignOp field op

prefixes :: Trie.Trie PrefixEntry
prefixes = prefixBuild $ execWriter $ do
	tell $ pure $ (["_"],) $ PrefixEntry "alias" Always $ \text ->
		(chunk text *>) $
		fmap (EField . flip FieldAccess [] . AliasName) $
		if T.null text
		then quoted
		else pure text
	tell $ pure $ ([""],) $ PrefixEntry "playlist name" MostPrecise $ \text ->
		(chunk text *>) $
		fmap (EField . flip FieldAccess [] . PlaylistName) $
		if T.null text
		then quoted
		else pure text
	tell $ pure $ (["p:", "pl:", "playlist:", "spotify:playlist:"],) $ PrefixEntry "playlist id" Always $ \text -> do
		chunk text
		guard $ T.all isAlphaNum text
		pure $ EField $ flip FieldAccess [] $ PlaylistId text
	tell $ pure $ (["spotify:user:"],) $ PrefixEntry "user playlist id" Always $ \text -> do
		chunk text
		[_user, "playlist", pl_id] <- pure $ T.splitOn ":" text
		pure $ EField $ flip FieldAccess [] $ PlaylistId pl_id
	tell $ pure $ (["t:", "tr:", "track:", "spotify:track:"],) $ PrefixEntry "track id" Always $ \text -> do
		chunk text
		guard $ T.all isAlphaNum text
		pure $ ECmd $ TrackId text
	tell $ pure $ (["al:", "album:", "spotify:album:"],) $ PrefixEntry "album id" Always $ \text -> do
		chunk text
		guard $ T.all isAlphaNum text
		pure $ ECmd $ AlbumId text
	tell $ pure $ (["ar:", "artist:", "spotify:artist:"],) $ PrefixEntry "artist id" Always $ \text -> do
		chunk text
		guard $ T.all isAlphaNum text
		pure $ ECmd $ ArtistId text
	tell $ pure $ (["playing", "current"],) $ PrefixEntry "playing" Exact $ const $
		pure $ EField $ FieldAccess Playing []
	tell $ pure $ (["playing_song", "current_song"],) $ PrefixEntry "playing song" Exact $ const $
		pure $ ECmd PlayingSong
	tell $ pure $ (["empty", "ε", "ø"],) $ PrefixEntry "empty" Exact $ const $
		pure $ ECmd Empty
	tell $ pure $ (["union"],) $ PrefixEntry "union" Exact $ const $
		fmap (ECmd . compoundUnion) $ ws *> compound
	tell $ pure $ (["intersect", "intersection"],) $ PrefixEntry "intersect" Exact $ const $
		fmap (ECmd . compoundIntersect) $ ws *> compound
	tell $ pure $ (["seq", "sequence"],) $ PrefixEntry "sequence" Exact $ const $
		fmap (ECmd . compoundSequence) $ ws *> compound
	
	for_ unaryOps $ \(names, op) -> do
		tell $ pure $ (names,) $ PrefixEntry (head names) Exact $ const $ do
			modify <- optional $ single '!' *> notFollowedBy (satisfy isOperatorChar)
			ws
			e <- exprPrecs !! 5
			assignableOp (isJust modify) op e

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

exprPrecs = scanl (flip ($))
	(   prefixParse prefixes (takeWhileP (Just "word") isWordChar <* notFollowedBy (satisfy isWordChar))
	<|> (char '(' *> ws *> expr <* ws <* char ')')
	)
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
