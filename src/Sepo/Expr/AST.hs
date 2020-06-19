module Sepo.Expr.AST where

import Data.Text

data Field
	= PlaylistId Text
	| PlaylistName Text
	| AliasName Text
	| Playing
	deriving (Show)

data FieldAccess = FieldAccess {
	fieldAccessField :: Field,
	fieldAccessAssignments :: [Cmd]
} deriving (Show)

data Cmd
	= Field FieldAccess
	| TrackId Text
	| AlbumId Text
	| ArtistId Text
	| PlayingSong
	| Empty
	| Seq Cmd Cmd
	| RevSeq Cmd Cmd
	| Concat Cmd Cmd
	| Subtract Cmd Cmd
	| Intersect Cmd Cmd
	| Unique Cmd
	| Shuffle Cmd
	deriving (Show)

assign :: FieldAccess -> Cmd -> FieldAccess
assign field cmd = field { fieldAccessAssignments = fieldAccessAssignments field ++ [cmd] }

assignOp :: FieldAccess -> (Cmd -> Cmd) -> FieldAccess
assignOp field op = assign field $ op $ Field $ field { fieldAccessAssignments = [] }

assConcat :: FieldAccess -> Cmd -> FieldAccess
assConcat field cmd = assign field $ Concat (Field $ field { fieldAccessAssignments = [] }) cmd

assSubtract :: FieldAccess -> Cmd -> FieldAccess
assSubtract field cmd = assign field $ Subtract (Field $ field { fieldAccessAssignments = [] }) cmd

assIntersect :: FieldAccess -> Cmd -> FieldAccess
assIntersect field cmd = assign field $ Intersect (Field $ field { fieldAccessAssignments = [] }) cmd

assUnique :: FieldAccess -> FieldAccess
assUnique field = assign field $ Unique (Field $ field { fieldAccessAssignments = [] })

assShuffle :: FieldAccess -> FieldAccess
assShuffle field = assign field $ Shuffle (Field $ field { fieldAccessAssignments = [] })

data Prec = PAssign | PPrefix | PSeq | PPosNeg | PIntersect | PPostfix | PUnit deriving (Eq, Ord, Show, Bounded)

parens :: Bool -> Text -> Text
parens True t = "(" <> t <> ")"
parens False t = t

class Reify a where
	reify :: Prec -> a -> Text

reifyQuoted :: Text -> Text
reifyQuoted =
	("'" <>) .
	(<> "'") .
	replace "\n" "\\n" .
	replace "\r" "\\r" .
	replace "\t" "\\t" .
	replace "'" "\\'" .
	replace "\\" "\\\\"

instance Reify Field where
	reify d (PlaylistId pl_id) = "spotify:playlist:" <> pl_id
	reify d (PlaylistName name) = reifyQuoted name
	reify d (AliasName name) = "_" <> reifyQuoted name
	reify d Playing = "playing"

instance Reify FieldAccess where
	reify d (FieldAccess f []) = reify d f
	reify d (FieldAccess f (cmd:cmds)) = parens (d > PAssign) $
		reify PSeq (FieldAccess f cmds) <> " = " <> reify PAssign cmd

instance Reify Cmd where
	reify d (Field f) = reify d f
	reify d (TrackId tr_id) = "spotify:track:" <> tr_id
	reify d (AlbumId al_id) = "spotify:album:" <> al_id
	reify d (ArtistId ar_id) = "spotify:artist:" <> ar_id
	reify d PlayingSong = "playing_song"
	reify d Empty = "empty"
	reify d (Seq a b) = parens (d > PSeq) $ reify PSeq a <> " *> " <> reify PSeq b
	reify d (RevSeq a b) = parens (d > PSeq) $ reify PSeq a <> " <* " <> reify PSeq b
	reify d (Concat a b) = parens (d > PPosNeg) $ reify PPosNeg a <> " + " <> reify PPosNeg b
	reify d (Subtract a b) = parens (d > PPosNeg) $ reify PPosNeg a <> " - " <> reify PIntersect b
	reify d (Intersect a b) = parens (d > PIntersect) $ reify PIntersect a <> " & " <> reify PIntersect b
	reify d (Unique a) = parens (d > PPrefix) $ "unique " <> reify PPrefix a
	reify d (Shuffle a) = parens (d > PPrefix) $ "shuffle " <> reify PPrefix a
