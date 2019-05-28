module Sepo.AST where

import Data.Text

data Field
	= PlaylistId Text
	| PlaylistName Text
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
	| Intersect Cmd Cmd
	| Subtract Cmd Cmd
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
