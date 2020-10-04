{-# LANGUAGE TemplateHaskell #-}

module Sepo.Runtime.Values where

import Control.Monad (join)
import Control.Monad.Fraxl (GEq(..), GCompare(..), GOrdering(..))
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(UntaggedValue))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.List (stripPrefix, sortOn)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Data.Type.Equality ((:~:)(Refl))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEnc
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Sepo.WebClient as HTTP

type MultiSet a = M.Map a Int
msToL :: MultiSet a -> [a]
msToL ms = M.assocs ms >>= uncurry (flip replicate)

data Playlist = Playlist {
	playlistId :: T.Text,
	playlistName :: T.Text,
	playlistSnapshotId :: T.Text
} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions {
	fieldLabelModifier = fmap toLower . fromJust . stripPrefix "playlist",
	sumEncoding = UntaggedValue
} 'Playlist)

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
instance Aeson.ToJSON Tracks where
	toJSON (Ordered tracks) = Aeson.Array $ V.fromList $ fmap Aeson.toJSON tracks
	toJSON (Unordered tracks) = Aeson.object $ fmap go $ M.toList tracks
		where go (track, count) = (trackId track, Aeson.object [
				("track", Aeson.toJSON track),
				("count", Aeson.toJSON count)
			])
	toEncoding (Ordered tracks) = AesonEnc.list Aeson.toEncoding tracks
	toEncoding (Unordered tracks) = encodeTracksSet tracks

encodeTracksSet :: MultiSet Track -> AesonEnc.Encoding
encodeTracksSet tracks = AesonEnc.dict AesonEnc.text id (foldl . go) $ M.toList tracks
	where go f acc (track, count) = f (trackId track)
		(AesonEnc.dict AesonEnc.text id
			(\f acc () ->
				f "track" (Aeson.toEncoding track) $
				f "count" (Aeson.toEncoding count) $
				acc)
			())
		acc

data Existing = ExPlaylist Playlist | ExAlbum Album | ExArtist Artist deriving (Show)
$(deriveJSON defaultOptions ''Existing)
data Value m = Value {
	tracks :: m Tracks,
	existing :: Maybe Existing
}

vEmpty :: Applicative m => Value m
vEmpty = Value (pure $ Unordered M.empty) Nothing

vConcat :: Applicative m => Value m -> Value m -> Value m
vConcat a b = flip Value Nothing $ flip fmap ((,) <$> tracks a <*> tracks b) $ \case
	(Ordered a, Ordered b) -> Ordered $ a ++ b
	(Ordered a, Unordered b) -> Ordered $ a ++ msToL b
	(Unordered a, Ordered b) -> Ordered $ msToL a ++ b
	(Unordered a, Unordered b) -> Unordered $ M.unionWith (+) a b

vUnique :: Functor m => Value m -> Value m
vUnique = flip Value Nothing . fmap (Unordered . M.map (const 1) . tracksSet) . tracks

vSortTrack :: Functor m => Value m -> Value m
vSortTrack = flip Value Nothing . fmap (Ordered . sortOn trackName . tracksList) . tracks

vSortAlbum :: Functor m => Value m -> Value m
vSortAlbum = flip Value Nothing . fmap (Ordered . sortOn (albumName . trackAlbum) . tracksList) . tracks

vSortArtist :: Functor m => Value m -> Value m
vSortArtist = flip Value Nothing . fmap (Ordered . sortOn (fmap artistName . trackArtists) . tracksList) . tracks

data Source a where
	SCurrentUser :: Source T.Text
	SCurrentUserPlaylists :: Source [Playlist]
	SPlaylist :: T.Text -> Source Playlist
	SPlaylistTracks :: T.Text -> Source [(Track, UTCTime)]
	SCurrentlyPlaying :: Source (Maybe (Maybe (HTTP.ContextType, T.Text), Track))
	STrack :: T.Text -> Source Track
	SAlbum :: T.Text -> Source Album
	SAlbumTracks :: T.Text -> Source [Track]
	SArtist :: T.Text -> Source Artist
	SArtistAlbums :: T.Text -> Source [Album]
	SSearchArtists :: T.Text -> Int -> Source (Bool, [Artist])
	SSearchAlbums :: T.Text -> Int -> Source (Bool, [Album])
	SSearchTracks :: T.Text -> Int -> Source (Bool, [Track])
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
	SSearchArtists aq ai `geq` SSearchArtists bq bi = bool (Just Refl) Nothing $ (aq, ai) == (bq, bi)
	SSearchAlbums aq ai `geq` SSearchAlbums bq bi = bool (Just Refl) Nothing $ (aq, ai) == (bq, bi)
	SSearchTracks aq ai `geq` SSearchTracks bq bi = bool (Just Refl) Nothing $ (aq, ai) == (bq, bi)
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
	SArtistAlbums _ `gcompare` _ = GLT
	SSearchArtists _ _ `gcompare` SCurrentUser = GGT
	SSearchArtists _ _ `gcompare` SCurrentUserPlaylists = GGT
	SSearchArtists _ _ `gcompare` SPlaylist _ = GGT
	SSearchArtists _ _ `gcompare` SPlaylistTracks _ = GGT
	SSearchArtists _ _ `gcompare` SCurrentlyPlaying = GGT
	SSearchArtists _ _ `gcompare` STrack _ = GGT
	SSearchArtists _ _ `gcompare` SAlbum _ = GGT
	SSearchArtists _ _ `gcompare` SAlbumTracks _ = GGT
	SSearchArtists _ _ `gcompare` SArtist _ = GGT
	SSearchArtists _ _ `gcompare` SArtistAlbums _ = GGT
	SSearchArtists aq ai `gcompare` SSearchArtists bq bi = case compare (aq, ai) (bq, bi) of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SSearchArtists _ _ `gcompare` _ = GLT
	SSearchAlbums _ _ `gcompare` SCurrentUser = GGT
	SSearchAlbums _ _ `gcompare` SCurrentUserPlaylists = GGT
	SSearchAlbums _ _ `gcompare` SPlaylist _ = GGT
	SSearchAlbums _ _ `gcompare` SPlaylistTracks _ = GGT
	SSearchAlbums _ _ `gcompare` SCurrentlyPlaying = GGT
	SSearchAlbums _ _ `gcompare` STrack _ = GGT
	SSearchAlbums _ _ `gcompare` SAlbum _ = GGT
	SSearchAlbums _ _ `gcompare` SAlbumTracks _ = GGT
	SSearchAlbums _ _ `gcompare` SArtist _ = GGT
	SSearchAlbums _ _ `gcompare` SArtistAlbums _ = GGT
	SSearchAlbums _ _ `gcompare` SSearchArtists _ _ = GGT
	SSearchAlbums aq ai `gcompare` SSearchAlbums bq bi = case compare (aq, ai) (bq, bi) of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	SSearchAlbums _ _ `gcompare` _ = GLT
	SSearchTracks _ _ `gcompare` SCurrentUser = GGT
	SSearchTracks _ _ `gcompare` SCurrentUserPlaylists = GGT
	SSearchTracks _ _ `gcompare` SPlaylist _ = GGT
	SSearchTracks _ _ `gcompare` SPlaylistTracks _ = GGT
	SSearchTracks _ _ `gcompare` SCurrentlyPlaying = GGT
	SSearchTracks _ _ `gcompare` STrack _ = GGT
	SSearchTracks _ _ `gcompare` SAlbum _ = GGT
	SSearchTracks _ _ `gcompare` SAlbumTracks _ = GGT
	SSearchTracks _ _ `gcompare` SArtist _ = GGT
	SSearchTracks _ _ `gcompare` SArtistAlbums _ = GGT
	SSearchTracks _ _ `gcompare` SSearchArtists _ _ = GGT
	SSearchTracks _ _ `gcompare` SSearchAlbums _ _ = GGT
	SSearchTracks aq ai `gcompare` SSearchTracks bq bi = case compare (aq, ai) (bq, bi) of
		LT -> GLT
		EQ -> GEQ
		GT -> GGT
	-- SSearchTracks _ _ `gcompare` _ = GLT
	-- a `gcompare` b = error $ show (a, b)

data Action a where
	APlaylistCreate :: HTTP.CreatePlaylist -> Action Playlist
	APlaylistReplace :: T.Text -> [Track] -> Action Playlist
	APlaylistAdd :: T.Text -> Maybe Int -> [Track] -> Action Playlist

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
