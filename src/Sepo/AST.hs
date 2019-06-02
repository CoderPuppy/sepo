module Sepo.AST where

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
	| Concat Cmd Cmd
	| Subtract Cmd Cmd
	| Intersect Cmd Cmd
	| Unique Cmd
	| Shuffle Cmd
	deriving (Show)

assign :: FieldAccess -> Cmd -> FieldAccess
assign field cmd = field { fieldAccessAssignments = fieldAccessAssignments field ++ [cmd] }

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

data Prec = PSeq | PPrefix | PPosNeg | PAnd | PPostfix | PUnit deriving (Eq, Ord, Show)

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
	reify d (FieldAccess f cmds) | d > PPrefix && d <= PPostfix =
		reify PUnit f <> mconcat (fmap (("!= " <>) . reify PUnit) cmds)
	reify d (FieldAccess f cmds) = parens (d > PPrefix) $
		reify PPostfix f <> mconcat (fmap ((" = " <>) . reify PPrefix) cmds)

instance Reify Cmd where
	reify d (Field f) = reify d f
	reify d (TrackId tr_id) = "spotify:track:" <> tr_id
	reify d (AlbumId al_id) = "spotify:album:" <> al_id
	reify d (ArtistId ar_id) = "spotify:artist:" <> ar_id
	reify d PlayingSong = "playing/song"
	reify d Empty = "empty"
	reify d (Seq a b) = parens (d > PSeq) $ reify PSeq a <> "; " <> reify PSeq b
	reify d (Concat a b) = parens (d > PPosNeg) $ reify PPosNeg a <> " + " <> reify PPosNeg b
	reify d (Subtract a b) = parens (d > PPosNeg) $ reify PPosNeg a <> " - " <> reify PAnd b
	reify d (Intersect a b) = parens (d > PAnd) $ reify PAnd a <> " & " <> reify PAnd b
	reify d (Unique a) = parens (d > PPrefix) $ "unique " <> reify PPrefix a
	reify d (Shuffle a) = parens (d > PPrefix) $ "shuffle " <> reify PPrefix a
