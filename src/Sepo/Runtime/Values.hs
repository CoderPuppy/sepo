{-# LANGUAGE TemplateHaskell #-}

module Sepo.Runtime.Values where

import Control.Monad (join)
import Control.Monad.Fraxl (GEq(..), GCompare(..), GOrdering(..))
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(UntaggedValue))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Data.Type.Equality ((:~:)(Refl))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Sepo.WebClient as HTTP

type MultiSet a = M.Map a Int
msToL :: MultiSet a -> [a]
msToL ms = M.assocs ms >>= uncurry (flip replicate)

data Playlist = Playlist {
	playlistId :: T.Text,
	playlistName :: T.Text,
	playlistSnapshotId :: T.Text
} deriving (Eq, Ord, Show)

data Artist = Artist {
	artistId :: T.Text,
	artistName :: T.Text
} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions {
	fieldLabelModifier = fmap toLower . fromJust . stripPrefix "artist",
	sumEncoding = UntaggedValue
} 'Artist)

data Album = Album {
	albumId :: T.Text,
	albumName :: T.Text,
	albumArtists :: [Artist]
} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions {
	fieldLabelModifier = fmap toLower . fromJust . stripPrefix "album",
	sumEncoding = UntaggedValue
} 'Album)

data Track = Track {
	trackId :: T.Text,
	trackName :: T.Text,
	trackAlbum :: Album,
	trackArtists :: [Artist],
	trackExplicit :: Bool
} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions {
	fieldLabelModifier = fmap toLower . fromJust . stripPrefix "track",
	sumEncoding = UntaggedValue
} 'Track)

data Tracks = Ordered [Track] | Unordered (MultiSet Track) deriving (Show)

data Existing = ExPlaylist Playlist | ExAlbum Album | ExArtist Artist deriving (Show)
data Value m = Value {
	tracks :: Thunk m Tracks,
	existing :: Maybe Existing
}

data Thunk m a = Strict a | Lazy (m a) deriving (Functor)
force :: Applicative m => Thunk m a -> m a
force (Strict v) = pure v
force (Lazy thunk) = thunk
joinThunk :: Monad m => Thunk m (m a) -> m (Thunk m a)
joinThunk (Strict m) = fmap Strict m
joinThunk (Lazy m) = pure $ Lazy $ join m
instance Applicative m => Applicative (Thunk m) where
	pure = Strict
	Strict f <*> Strict a = Strict $ f a
	Strict f <*> Lazy a = Lazy $ f <$> a
	Lazy f <*> Strict a = Lazy $ ($ a) <$> f
	Lazy f <*> Lazy a = Lazy $ f <*> a

data Source a where
	SCurrentUser :: Source T.Text
	SCurrentUserPlaylists :: Source [Playlist]
	SPlaylist :: T.Text -> Source Playlist
	SPlaylistTracks :: T.Text -> Source [(Track, UTCTime)]
	SCurrentlyPlaying :: Source (Maybe (HTTP.ContextType, T.Text), Track)
	STrack :: T.Text -> Source Track
	SAlbum :: T.Text -> Source Album
	SAlbumTracks :: T.Text -> Source [Track]
	SArtist :: T.Text -> Source Artist
	SArtistAlbums :: T.Text -> Source [Album]
deriving instance Show (Source a)
instance GEq Source where
	SCurrentUser `geq` SCurrentUser = Just Refl
	SCurrentUserPlaylists `geq` SCurrentUserPlaylists = Just Refl
	SPlaylist a `geq` SPlaylist b = bool (Just Refl) Nothing $ a == b
	SPlaylistTracks a `geq` SPlaylistTracks b = bool (Just Refl) Nothing $ a == b
	SCurrentlyPlaying `geq` SCurrentlyPlaying = Just Refl
	STrack a `geq` STrack b = bool (Just Refl) Nothing $ a == b
	SAlbum a `geq` SAlbum b = bool (Just Refl) Nothing $ a == b
	SAlbumTracks a `geq` SAlbumTracks b = bool (Just Refl) Nothing $ a == b
	SArtist a `geq` SArtist b = bool (Just Refl) Nothing $ a == b
	SArtistAlbums a `geq` SArtistAlbums b = bool (Just Refl) Nothing $ a == b
	_ `geq` _ = Nothing
instance GCompare Source where
	SCurrentUser `gcompare` SCurrentUser = GEQ
	SCurrentUser `gcompare` _ = GLT
	SCurrentUserPlaylists`gcompare` SCurrentUser = GGT
	SCurrentUserPlaylists`gcompare` SCurrentUserPlaylists = GEQ
	SCurrentUserPlaylists`gcompare` _ = GLT
	SPlaylist _ `gcompare` SCurrentUser = GGT
	SPlaylist _ `gcompare` SCurrentUserPlaylists = GGT
	SPlaylist a `gcompare` SPlaylist b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SPlaylist _ `gcompare` _ = GLT
	SPlaylistTracks _ `gcompare` SCurrentUser = GGT
	SPlaylistTracks _ `gcompare` SCurrentUserPlaylists = GGT
	SPlaylistTracks _ `gcompare` SPlaylist _ = GGT
	SPlaylistTracks a `gcompare` SPlaylistTracks b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SPlaylistTracks _ `gcompare` _ = GLT
	SCurrentlyPlaying `gcompare` SCurrentUser = GGT
	SCurrentlyPlaying `gcompare` SCurrentUserPlaylists = GGT
	SCurrentlyPlaying `gcompare` SPlaylist _ = GGT
	SCurrentlyPlaying `gcompare` SPlaylistTracks _ = GGT
	SCurrentlyPlaying `gcompare` SCurrentlyPlaying = GEQ
	SCurrentlyPlaying `gcompare` _ = GLT
	STrack _ `gcompare` SCurrentUser = GGT
	STrack _ `gcompare` SCurrentUserPlaylists = GGT
	STrack _ `gcompare` SPlaylist _ = GGT
	STrack _ `gcompare` SPlaylistTracks _ = GGT
	STrack _ `gcompare` SCurrentlyPlaying = GGT
	STrack a `gcompare` STrack b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	STrack _ `gcompare` _ = GLT
	SAlbum _ `gcompare` SCurrentUser = GGT
	SAlbum _ `gcompare` SCurrentUserPlaylists = GGT
	SAlbum _ `gcompare` SPlaylist _ = GGT
	SAlbum _ `gcompare` SPlaylistTracks _ = GGT
	SAlbum _ `gcompare` SCurrentlyPlaying = GGT
	SAlbum _ `gcompare` STrack _ = GGT
	SAlbum a `gcompare` SAlbum b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SAlbum _ `gcompare` _ = GLT
	SAlbumTracks _ `gcompare` SCurrentUser = GGT
	SAlbumTracks _ `gcompare` SCurrentUserPlaylists = GGT
	SAlbumTracks _ `gcompare` SPlaylist _ = GGT
	SAlbumTracks _ `gcompare` SPlaylistTracks _ = GGT
	SAlbumTracks _ `gcompare` SCurrentlyPlaying = GGT
	SAlbumTracks _ `gcompare` STrack _ = GGT
	SAlbumTracks _ `gcompare` SAlbum _ = GGT
	SAlbumTracks a `gcompare` SAlbumTracks b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SAlbumTracks _ `gcompare` _ = GLT
	SArtist _ `gcompare` SCurrentUser = GGT
	SArtist _ `gcompare` SCurrentUserPlaylists = GGT
	SArtist _ `gcompare` SPlaylist _ = GGT
	SArtist _ `gcompare` SPlaylistTracks _ = GGT
	SArtist _ `gcompare` SCurrentlyPlaying = GGT
	SArtist _ `gcompare` STrack _ = GGT
	SArtist _ `gcompare` SAlbum _ = GGT
	SArtist _ `gcompare` SAlbumTracks _ = GGT
	SArtist a `gcompare` SArtist b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SArtist _ `gcompare` _ = GLT
	SArtistAlbums _ `gcompare` SCurrentUser = GGT
	SArtistAlbums _ `gcompare` SCurrentUserPlaylists = GGT
	SArtistAlbums _ `gcompare` SPlaylist _ = GGT
	SArtistAlbums _ `gcompare` SPlaylistTracks _ = GGT
	SArtistAlbums _ `gcompare` SCurrentlyPlaying = GGT
	SArtistAlbums _ `gcompare` STrack _ = GGT
	SArtistAlbums _ `gcompare` SAlbum _ = GGT
	SArtistAlbums _ `gcompare` SAlbumTracks _ = GGT
	SArtistAlbums _ `gcompare` SArtist _ = GGT
	SArtistAlbums a `gcompare` SArtistAlbums b = case compare a b of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	-- SArtistAlbums _ `gcompare` _ = GLT
	-- a `gcompare` b = error $ show (a, b)

tracksList :: Tracks -> [Track]
tracksList (Ordered tracks) = tracks
tracksList (Unordered tracks) = msToL tracks

tracksSet :: Tracks -> MultiSet Track
tracksSet (Ordered tracks) = M.fromListWith (+) $ fmap (, 1) tracks
tracksSet (Unordered tracks) = tracks

httpPlaylist :: HTTP.Playlist -> Playlist
httpPlaylist playlist = Playlist {
		playlistId = HTTP.playlistId playlist,
		playlistName = HTTP.playlistName playlist,
		playlistSnapshotId = HTTP.playlistSnapshotId playlist
	}

httpPlaylistS :: HTTP.PlaylistS -> Playlist
httpPlaylistS playlist = Playlist {
		playlistId = HTTP.playlistSId playlist,
		playlistName = HTTP.playlistSName playlist,
		playlistSnapshotId = HTTP.playlistSSnapshotId playlist
	}

httpArtistS :: HTTP.ArtistS -> Artist
httpArtistS artist = Artist {
		artistId = HTTP.artistSId artist,
		artistName = HTTP.artistSName artist
	}

httpAlbumS :: HTTP.AlbumS -> Album
httpAlbumS album = Album {
		albumId = HTTP.albumSId album,
		albumName = HTTP.albumSName album,
		albumArtists = fmap httpArtistS $ HTTP.albumSArtists album
	}

httpAlbum :: HTTP.Album -> Album
httpAlbum album = Album {
		albumId = HTTP.albumId album,
		albumName = HTTP.albumName album,
		albumArtists = fmap httpArtistS $ HTTP.albumArtists album
	}

httpTrackS :: Album -> HTTP.TrackS -> Track
httpTrackS album track = Track {
		trackId = HTTP.trackSId track,
		trackName = HTTP.trackSName track,
		trackAlbum = album,
		trackArtists = fmap httpArtistS $ HTTP.trackSArtists track,
		trackExplicit = HTTP.trackSExplicit track
	}

httpTrack :: HTTP.Track -> Track
httpTrack track = Track {
		trackId = HTTP.trackId track,
		trackName = HTTP.trackName track,
		trackAlbum = httpAlbumS $ HTTP.trackAlbum track,
		trackArtists = fmap httpArtistS $ HTTP.trackArtists track,
		trackExplicit = HTTP.trackExplicit track
	}
