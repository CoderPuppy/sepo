module Sepo.Expr.AST where

import Data.Bool (bool)
import System.FilePath
import qualified Data.Text as T

data Field
	= PlaylistId T.Text
	| PlaylistName T.Text
	| AliasName T.Text
	| Playing
	| File FilePath
	deriving (Show)

data FieldAccess = FieldAccess {
	fieldAccessField :: Field,
	fieldAccessAssignments :: [Cmd]
} deriving (Show)

data Cmd
	= Field FieldAccess
	| TrackId T.Text
	| AlbumId T.Text
	| ArtistId T.Text
	| PlayingSong
	| MyPlaylists
	| Empty
	| Seq Cmd Cmd
	| RevSeq Cmd Cmd
	| Concat Cmd Cmd
	| Subtract Cmd Cmd
	| Intersect Cmd Cmd
	| Unique Cmd
	| Shuffle Cmd
	| Expand Cmd
	| SortTrack Cmd
	| SortAlbum Cmd
	| SortArtist Cmd
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

parens :: Bool -> T.Text -> T.Text
parens True t = "(" <> t <> ")"
parens False t = t

class Reify a where
	reify :: Prec -> a -> T.Text

reifyQuoted :: T.Text -> T.Text
reifyQuoted =
	("'" <>) .
	(<> "'") .
	T.replace "\n" "\\n" .
	T.replace "\r" "\\r" .
	T.replace "\t" "\\t" .
	T.replace "'" "\\'" .
	T.replace "\\" "\\\\"

instance Reify Field where
	reify d (PlaylistId pl_id) = "spotify:playlist:" <> pl_id
	reify d (PlaylistName name) = reifyQuoted name
	reify d (AliasName name) = "_" <> reifyQuoted name
	reify d Playing = "playing"
	reify d (File path) = if
		| Just tail <- T.stripPrefix "/" path' -> "/" <> reifyQuoted tail
		| Just tail <- T.stripPrefix "./" path' -> "./" <> reifyQuoted tail
		| otherwise -> "./" <> reifyQuoted path'
		where path' = T.pack path

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
	reify d MyPlaylists = "my_playlists"
	reify d Empty = "empty"
	reify d (Seq a b) = parens (d > PSeq) $ reify PSeq a <> " *> " <> reify PSeq b
	reify d (RevSeq a b) = parens (d > PSeq) $ reify PSeq a <> " <* " <> reify PSeq b
	reify d (Concat a b) = parens (d > PPosNeg) $ reify PPosNeg a <> " + " <> reify PPosNeg b
	reify d (Subtract a b) = parens (d > PPosNeg) $ reify PPosNeg a <> " - " <> reify PIntersect b
	reify d (Intersect a b) = parens (d > PIntersect) $ reify PIntersect a <> " & " <> reify PIntersect b
	reify d (Unique a) = parens (d > PPrefix) $ "unique " <> reify PPrefix a
	reify d (Shuffle a) = parens (d > PPrefix) $ "shuffle " <> reify PPrefix a
	reify d (Expand a) = parens (d > PPrefix) $ "expand " <> reify PPrefix a
	reify d (SortTrack a) = parens (d > PPrefix) $ "sort_track " <> reify PPrefix a
	reify d (SortAlbum a) = parens (d > PPrefix) $ "sort_album " <> reify PPrefix a
	reify d (SortArtist a) = parens (d > PPrefix) $ "sort_artist " <> reify PPrefix a
