{-# LANGUAGE TemplateHaskell #-}

module Sepo.Runtime.Values where

import Control.Monad (join)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(UntaggedValue))
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
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
