module Sepo.WebClient where

import Control.Arrow
import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Retry
import Data.Aeson
import Data.List (find)
import Data.Maybe
import Data.Proxy
import Data.Time.Clock (UTCTime)
import GHC.Exts (IsList(..))
import GHC.IORef (atomicSwapIORef)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status
import Servant.API
import Servant.Client hiding (Client)
import System.IO (stderr, hPutStrLn)
import Text.Read (readMaybe)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment (getEnv)
import UnliftIO.Exception (bracket_)
import UnliftIO.IORef
import UnliftIO.MVar
import Web.FormUrlEncoded (ToForm(..))
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

clientEnv :: Manager -> ClientEnv
clientEnv man = mkClientEnv man (BaseUrl Https "api.spotify.com" 443 "/v1")

data Ctx = Ctx {
	ctxMan :: Manager,
	ctxPath :: FilePath,
	ctxTokenRef :: IORef (MVar T.Text),
	ctxTokenRefreshRef :: IORef Bool,
	ctxClientId :: T.Text,
	ctxClientSecret :: T.Text,
	ctxRefreshToken :: T.Text,
	ctxQSem :: QSem,
	ctxHold :: IORef (Maybe (MVar ()))
}

start :: MonadIO m => m Ctx
start = do
	ctxMan <- newTlsManager
	home <- getEnv "HOME"
	let ctxPath = home <> "/.config/sepo"
	token <- liftIO $ T.readFile $ ctxPath <> "/token"
	ctxTokenRef <- newIORef =<< newMVar token
	ctxTokenRefreshRef <- newIORef False
	ctxClientId <- fmap (head . T.lines) $ liftIO $ T.readFile $ ctxPath <> "/client-id"
	ctxClientSecret <- fmap (head . T.lines) $ liftIO $ T.readFile $ ctxPath <> "/client-secret"
	ctxRefreshToken <- fmap (head . T.lines) $ liftIO $ T.readFile $ ctxPath <> "/refresh-token"
	ctxQSem <- liftIO $ newQSem 10 -- TODO: config
	ctxHold <- newIORef Nothing
	pure $ Ctx {..}

retryHold :: MonadIO m =>
	RetryPolicyM m -> IORef (Maybe (MVar ())) ->
	(RetryStatus -> a -> m RetryAction) ->
	(RetryStatus -> m a) ->
	m a
retryHold policy hold check act = go Nothing defaultRetryStatus
	where
		go ourHold status = do
			case ourHold of
				Nothing -> readIORef hold >>= maybe (pure ()) readMVar
				Just _ -> pure ()
			res <- act status
			action <- check status res
			status' <- case action of
				DontRetry -> pure Nothing
				ConsultPolicy -> applyPolicy policy status
				ConsultPolicyOverrideDelay delay -> applyPolicy (RetryPolicyM $ fmap (fmap $ fmap $ const delay) $ getRetryPolicyM policy) status
			case status' of
				Just status' -> do
					ourHold <- flip fromMaybe (fmap (pure . Just) ourHold) $ do
						ourHold <- newEmptyMVar
						atomicModifyIORef hold $ \case
							Nothing -> (Just ourHold, Just ourHold)
							Just hold -> (Just hold, Nothing)
					maybe (pure ()) threadDelay $ rsPreviousDelay status'
					go ourHold status'
				Nothing -> do
					flip (maybe (pure ())) ourHold $ \ourHold -> do
						atomicWriteIORef hold Nothing
						putMVar ourHold ()
					pure res

run :: MonadIO m => Ctx -> (Client -> ClientM a) -> m (Either ClientError a)
run ctx m = liftIO $
	bracket_ (waitQSem $ ctxQSem ctx) (signalQSem $ ctxQSem ctx) $
	do
		token <- (>>= readMVar) $ readIORef $ ctxTokenRef ctx
		retryHold
			(fibonacciBackoff 1_000_000) -- 1 second
			(ctxHold ctx)
			(const $ pure . \case
				(Left (FailureResponse req res)) | responseStatusCode res == tooManyRequests429 ->
					maybe ConsultPolicy ConsultPolicyOverrideDelay $ do
						(_, val) <- find ((== "retry-after") . fst) $ responseHeaders res
						seconds <- readMaybe $ T.unpack $ T.decodeUtf8 val
						pure $ seconds * 1_000_000
				_ -> DontRetry)
			$ const $ do
				res <- liftIO $ runClientM (m $ makeClient token) (clientEnv $ ctxMan ctx)
				case res of
					Left (FailureResponse req res) | responseStatusCode res == unauthorized401 -> do
						liftIO $ hPutStrLn stderr $ "refreshing access token because a response failed with status 401: " <> show res
						refreshAccessToken ctx >>= \case
							Left err -> pure $ Left err
							Right () -> run ctx m
					res -> pure res

run_ :: (MonadIO m, MonadFail m) => Ctx -> (Client -> ClientM a) -> m a
run_ ctx m = run ctx m >>= \case
	Left err -> fail $ show err
	Right v -> pure v

refreshAccessToken :: MonadIO m => Ctx -> m (Either ClientError ())
refreshAccessToken ctx = liftIO (atomicSwapIORef (ctxTokenRefreshRef ctx) True) >>= \case
	True -> pure $ Right ()
	False -> do
		liftIO $ hPutStrLn stderr "refreshing access token"
		var <- newEmptyMVar
		atomicWriteIORef (ctxTokenRef ctx) var
		res <- liftIO $ runClientM
			(client (Proxy :: Proxy AuthAPI)
				(("Basic " <>) $ T.decodeUtf8 $ B64.encode $ T.encodeUtf8 $ ctxClientId ctx <> ":" <> ctxClientSecret ctx)
				(RefreshToken $ ctxRefreshToken ctx))
			(mkClientEnv (ctxMan ctx) (BaseUrl Https "accounts.spotify.com" 443 "/api"))
		case res of
			Left err -> pure $ Left err
			Right (RefreshedToken {..}) -> do
				liftIO $ T.writeFile (ctxPath ctx <> "/token") refreshedTokenAccessToken
				putMVar var refreshedTokenAccessToken
				atomicWriteIORef (ctxTokenRefreshRef ctx) False
				pure $ Right ()

type AuthAPI = "token" :> Header' '[Required, Strict] "Authorization" T.Text :> ReqBody '[FormUrlEncoded] RefreshToken :> Post '[JSON] RefreshedToken

newtype RefreshToken = RefreshToken { unRefreshToken :: T.Text } deriving (Show)
instance ToForm RefreshToken where
	toForm (RefreshToken token) = fromList [
			("grant_type", "refresh_token"),
			("refresh_token", token)
		]

data RefreshedToken = RefreshedToken {
	refreshedTokenAccessToken :: T.Text,
	refreshedTokenTokenTupe :: T.Text,
	refreshedTokenExpiresIn :: Int,
	refreshedTokenScope :: T.Text
} deriving (Show)
instance FromJSON RefreshedToken where
	parseJSON = withObject "RefreshedToken" $ \o -> RefreshedToken
		<$> o .: "access_token"
		<*> o .: "token_type"
		<*> o .: "expires_in"
		<*> o .: "scope"

type PagedPath a = QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] (Paging a)
type PagedFn a = Maybe Int -> Maybe Int -> ClientM (Paging a)

type SpotifyAPI = Header' '[Required, Strict] "Authorization" T.Text :>
	(    "me" :> Get '[JSON] User
	:<|> "me" :> "playlists" :> PagedPath PlaylistS

	:<|> "playlists" :> Capture "playlist_id" T.Text :> Get '[JSON] Playlist
	:<|> "playlists" :> Capture "playlist_id" T.Text :> "tracks" :> PagedPath PlaylistTrack

	:<|> "artists" :> Capture "artist_id" T.Text :> Get '[JSON] ArtistS -- TODO: really a full Artist
	:<|> "artists" :> QueryParam' '[Required, Strict] "ids" ArtistIds :> Get '[JSON] Artists
	:<|> "artists" :> Capture "artist_id" T.Text :> "albums" :> PagedPath AlbumS -- TODO: include_groups parameter

	:<|> "albums" :> Capture "album_id" T.Text :> Get '[JSON] Album
	:<|> "albums" :> QueryParam' '[Required, Strict] "ids" AlbumIds :> Get '[JSON] Albums
	:<|> "albums" :> Capture "album_id" T.Text :> "tracks" :> PagedPath TrackS

	:<|> "tracks" :> Capture "track_id" T.Text :> Get '[JSON] Track
	:<|> "tracks" :> QueryParam' '[Required, Strict] "ids" TrackIds :> Get '[JSON] Tracks

	:<|> "users" :> Capture "user_id" T.Text :> "playlists" :> ReqBody '[JSON] CreatePlaylist :> Post '[JSON] Playlist
	:<|> "playlists" :> Capture "playlist_id" T.Text :> ReqBody '[JSON] ChangePlaylistDetails :> Put '[JSON] ()

	:<|> "playlists" :> Capture "playlist_id" T.Text :> "tracks" :> ReqBody '[JSON] AddTracks :> Post '[JSON] SnapshotResp
	:<|> "playlists" :> Capture "playlist_id" T.Text :> "tracks" :> ReqBody '[JSON] RemoveTracks :> Delete '[JSON] SnapshotResp
	:<|> "playlists" :> Capture "playlist_id" T.Text :> "tracks" :> ReqBody '[JSON] ReorderTracks :> Put '[JSON] SnapshotResp
	:<|> "playlists" :> Capture "playlist_id" T.Text :> "tracks" :> ReqBody '[JSON] ReplaceTracks :> Put '[JSON] SnapshotResp

	:<|> "me" :> "player" :> Get '[JSON] CurrentlyPlaying
	:<|> "me" :> "player" :> "play" :> ReqBody '[JSON] Play :> Put '[JSON] ()

	:<|> "search" :> QueryParam' '[Required, Strict] "type" SearchTypeArtist :> QueryParam' '[Required, Strict] "q" T.Text :> QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] SearchArtists
	:<|> "search" :> QueryParam' '[Required, Strict] "type" SearchTypeAlbum :> QueryParam' '[Required, Strict] "q" T.Text :> QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] SearchAlbums
	:<|> "search" :> QueryParam' '[Required, Strict] "type" SearchTypeTrack :> QueryParam' '[Required, Strict] "q" T.Text :> QueryParam "offset" Int :> QueryParam "limit" Int :> Get '[JSON] SearchTracks
	)

data Client = Client {
	getUser :: ClientM User,
	getPlaylists :: PagedFn PlaylistS,

	getPlaylist :: T.Text -> ClientM Playlist,
	getPlaylistTracks :: T.Text -> PagedFn PlaylistTrack,

	getArtist :: T.Text -> ClientM ArtistS,
	getArtists :: ArtistIds -> ClientM Artists,
	getArtistAlbums :: T.Text -> PagedFn AlbumS,

	getAlbum :: T.Text -> ClientM Album,
	getAlbums :: AlbumIds -> ClientM Albums,
	getAlbumTracks :: T.Text -> PagedFn TrackS,

	getTrack :: T.Text -> ClientM Track,
	getTracks :: TrackIds -> ClientM Tracks,

	createPlaylist :: T.Text -> CreatePlaylist -> ClientM Playlist,
	changePlaylistDetails :: T.Text -> ChangePlaylistDetails -> ClientM (),

	addTracks :: T.Text -> AddTracks -> ClientM SnapshotResp,
	removeTracks :: T.Text -> RemoveTracks -> ClientM SnapshotResp,
	reorderTracks :: T.Text -> ReorderTracks -> ClientM SnapshotResp,
	replaceTracks :: T.Text -> ReplaceTracks -> ClientM SnapshotResp,

	getCurrentlyPlaying :: ClientM CurrentlyPlaying,
	play :: Play -> ClientM (), -- requires premium

	searchArtists :: SearchTypeArtist -> T.Text -> Maybe Int -> Maybe Int -> ClientM SearchArtists,
	searchAlbums :: SearchTypeAlbum -> T.Text -> Maybe Int -> Maybe Int -> ClientM SearchAlbums,
	searchTracks :: SearchTypeTrack -> T.Text -> Maybe Int -> Maybe Int -> ClientM SearchTracks
}

makeClient token = Client {..}
	where getUser :<|> getPlaylists :<|>
		getPlaylist :<|> getPlaylistTracks :<|>
		getArtist :<|> getArtists :<|> getArtistAlbums :<|>
		getAlbum :<|> getAlbums :<|> getAlbumTracks :<|>
		getTrack :<|> getTracks :<|>
		createPlaylist :<|> changePlaylistDetails :<|>
		addTracks :<|> removeTracks :<|> reorderTracks :<|> replaceTracks :<|>
		getCurrentlyPlaying :<|> play :<|>
		searchArtists :<|> searchAlbums :<|> searchTracks = client spotifyAPI $ "Bearer " <> token

spotifyAPI :: Proxy SpotifyAPI
spotifyAPI = Proxy

getAllPaged_1 :: PagedFn a -> Maybe Int -> Paging a -> [a] -> ClientM [a]
getAllPaged_1 fn limit paging items =
	if isJust $ pagingNext paging
	then getAllPaged_2 fn limit (pagingOffset paging + pagingLimit paging) items'
	else pure $ reverse items'
	where items' = reverse (pagingItems paging) ++ items

getAllPaged_2 :: PagedFn a -> Maybe Int -> Int -> [a] -> ClientM [a]
getAllPaged_2 fn limit offset items = do
	paging <- fn (Just offset) limit
	getAllPaged_1 fn limit paging items

getAllPaged :: PagedFn a -> Maybe Int -> ClientM [a]
getAllPaged fn limit= getAllPaged_2 fn limit 0 []

getAllPagedContinue :: PagedFn a -> Maybe Int -> Paging a -> ClientM [a]
getAllPagedContinue fn limit paging = getAllPaged_1 fn limit paging []

data Paging a = Paging {
	pagingHref :: T.Text,
	pagingItems :: [a],
	pagingLimit :: Int,
	pagingNext :: Maybe T.Text,
	pagingOffset :: Int,
	pagingPrevious :: Maybe T.Text,
	pagingTotal :: Int
} deriving (Show)
instance FromJSON a => FromJSON (Paging a) where
	parseJSON (Object o) = Paging
		<$> o .: "href"
		<*> o .: "items"
		<*> o .: "limit"
		<*> o .: "next"
		<*> o .: "offset"
		<*> o .: "previous"
		<*> o .: "total"
	parseJSON _ = mempty

data Playlist = Playlist {
	playlistCollaborative :: Bool,
	playlistDescription :: Maybe T.Text,
	playlistExternalUrls :: M.Map T.Text T.Text,
	-- playlistFollowers :: Followers,
	playlistHref :: T.Text,
	playlistId :: T.Text,
	-- playlistImages :: [Image],
	playlistName :: T.Text,
	playlistOwner :: User,
	playlistPublic :: Bool,
	playlistSnapshotId :: T.Text,
	playlistTracks :: Paging PlaylistTrack,
	playlistURI :: T.Text
} deriving (Show)
instance FromJSON Playlist where
	parseJSON = withObject "Playlist" $ \o -> Playlist
		<$> o .: "collaborative"
		<*> o .: "description"
		<*> o .: "external_urls"
		-- <*> o .: "followers"
		<*> o .: "href"
		<*> o .: "id"
		-- <*> o .: "images"
		<*> o .: "name"
		<*> o .: "owner"
		<*> o .: "public"
		<*> o .: "snapshot_id"
		<*> o .: "tracks"
		<*> o .: "uri"

data PlaylistS = PlaylistS {
	playlistSCollaborative :: Bool,
	playlistSExternalUrls :: M.Map T.Text T.Text,
	playlistSHref :: T.Text,
	playlistSId :: T.Text,
	-- playlistImages :: [Image],
	playlistSName :: T.Text,
	playlistSOwner :: User,
	playlistSPublic :: Bool,
	playlistSSnapshotId :: T.Text,
	-- playlistSTracks :: Tracks,
	playlistSTracksHref :: T.Text,
	playlistSTracksTotal :: Int,
	playlistSURI :: T.Text
} deriving (Show)
instance FromJSON PlaylistS where
	parseJSON (Object o) = PlaylistS
		<$> o .: "collaborative"
		<*> o .: "external_urls"
		<*> o .: "href"
		<*> o .: "id"
		-- <*> o .: "images"
		<*> o .: "name"
		<*> o .: "owner"
		<*> o .: "public"
		<*> o .: "snapshot_id"
		-- <*> o .: "tracks"
		<*> (o .: "tracks" >>= withObject "tracks" (.: "href"))
		<*> (o .: "tracks" >>= withObject "tracks" (.: "total"))
		<*> o .: "uri"
	parseJSON _ = mempty

data PlaylistTrack = PlaylistTrack {
	playlistTrackAddedAt :: UTCTime,
	-- playlistTrackAddedBy :: User,
	playlistTrackIsLocal :: Bool,
	playlistTrack :: Track
} deriving (Show)
instance FromJSON PlaylistTrack where
	parseJSON = withObject "PlaylistTrack" $ \o -> PlaylistTrack
		<$> o .: "added_at"
		-- <*> o .: "added_by"
		<*> o .: "is_local"
		<*> o .: "track"

newtype TrackIds = TrackIds { unTrackIds :: [T.Text] } deriving (Show)
instance ToHttpApiData TrackIds where
	toQueryParam (TrackIds ids) = T.intercalate "," ids

newtype Tracks = Tracks { unTracks :: [Track] } deriving (Show)
instance FromJSON Tracks where
	parseJSON = withObject "Tracks" $ \o -> Tracks <$> o.: "tracks"

data Track = Track {
	trackAlbum :: AlbumS,
	trackArtists :: [ArtistS],
	trackAvailableMarkets :: S.Set T.Text,
	trackDiscNumber :: Int,
	trackDurationMS :: Int,
	trackExplicit :: Bool,
	trackExternalIds :: M.Map T.Text T.Text,
	trackExternalUrls :: M.Map T.Text T.Text,
	trackHref :: T.Text,
	trackId :: T.Text,
	-- trackIsPlayable :: Bool,
	-- trackLinkedFrom :: LinkedTrack,
	-- trackRestrictions :: { reason :: T.Text },
	trackName :: T.Text,
	trackPopularity :: Int,
	trackPreviewUrl :: Maybe T.Text,
	trackNumber :: Int,
	trackURI :: T.Text
} deriving (Show)
instance FromJSON Track where
	parseJSON = withObject "Track" $ \o -> Track
		<$> o .: "album"
		<*> o .: "artists"
		<*> o .: "available_markets"
		<*> o .: "disc_number"
		<*> o .: "duration_ms"
		<*> o .: "explicit"
		<*> o .: "external_ids"
		<*> o .: "external_urls"
		<*> o .: "href"
		<*> o .: "id"
		-- <*> o .: "is_playable"
		-- <*> o .: "linked_from"
		-- <*> o .: "restrictions"
		<*> o .: "name"
		<*> o .: "popularity"
		<*> o .: "preview_url"
		<*> o .: "track_number"
		<*> o .: "uri"

data AlbumGroup = AGAlbum | AGSingle | AGCompilation | AGAppearsOn deriving (Show)
instance FromJSON AlbumGroup where
	parseJSON = withText "AlbumGroup" $ \case
		"album" -> pure AGAlbum
		"single" -> pure AGSingle
		"compilation" -> pure AGCompilation
		"appears_on" -> pure AGAppearsOn
		_ -> mempty

data AlbumType = ATAlbum | ATSingle | ATCompilation deriving (Show)
instance FromJSON AlbumType where
	parseJSON = withText "AlbumType" $ \case
		"album" -> pure ATAlbum
		"single" -> pure ATSingle
		"compilation" -> pure ATCompilation
		_ -> mempty

newtype AlbumIds = AlbumIds { unAlbumIds :: [T.Text] } deriving (Show)
instance ToHttpApiData AlbumIds where
	toQueryParam (AlbumIds ids) = T.intercalate "," ids

newtype Albums = Albums { unAlbums :: [Album] } deriving (Show)
instance FromJSON Albums where
	parseJSON = withObject "Albums" $ \o -> Albums <$> o.: "albums"

data Album = Album {
	albumType :: AlbumType,
	albumArtists :: [ArtistS],
	albumAvailableMarkets :: S.Set T.Text,
	-- albumCopyrights :: [Copyright],
	albumExternalIds :: M.Map T.Text T.Text,
	albumExternalUrls :: M.Map T.Text T.Text,
	albumGenres :: S.Set T.Text,
	albumHref :: T.Text,
	albumId :: T.Text,
	-- albumImages :: [Image],
	albumLabel :: T.Text,
	albumName :: T.Text,
	albumPopularity :: Int,
	albumReleaseData :: T.Text, -- TODO
	-- albumReleaseDataPrecision :: T.Text,
	albumTracks :: Paging TrackS,
	albumURI :: T.Text
} deriving (Show)
instance FromJSON Album where
	parseJSON = withObject "Album" $ \o -> Album
		<$> o .: "album_type"
		<*> o .: "artists"
		<*> o .: "available_markets"
		-- <*> o .: "copyrights"
		<*> o .: "external_ids"
		<*> o .: "external_urls"
		<*> o .: "genres"
		<*> o .: "href"
		<*> o .: "id"
		-- <*> o .: "images"
		<*> o .: "label"
		<*> o .: "name"
		<*> o .: "popularity"
		<*> o .: "release_date"
		-- <*> o .: "release_date_precision"
		-- <*> o .: "restrictions"
		<*> o .: "tracks"
		<*> o .: "uri"

data AlbumS = AlbumS {
	albumSGroup :: Maybe AlbumGroup,
	albumSType :: AlbumType,
	albumSArtists :: [ArtistS],
	albumSAvailableMarkets :: S.Set T.Text,
	albumSExternalUrls :: M.Map T.Text T.Text,
	albumSHref :: T.Text,
	albumSId :: T.Text,
	-- albumSImages :: [Image],
	albumSName :: T.Text,
	albumSReleaseData :: T.Text, -- TODO
	-- albumSReleaseDataPrecision :: T.Text,
	-- albumSRestrictions :: { reason :: T.Text },
	albumSURI :: T.Text
} deriving (Show)
instance FromJSON AlbumS where
	parseJSON = withObject "AlbumS" $ \o -> AlbumS
		<$> o .:? "album_group"
		<*> o .: "album_type"
		<*> o .: "artists"
		<*> o .: "available_markets"
		<*> o .: "external_urls"
		<*> o .: "href"
		<*> o .: "id"
		-- <*> o .: "images"
		<*> o .: "name"
		<*> o .: "release_date"
		-- <*> o .: "release_date_precision"
		-- <*> o .: "restrictions"
		<*> o .: "uri"

newtype ArtistIds = ArtistIds { unArtistIds :: [T.Text] } deriving (Show)
instance ToHttpApiData ArtistIds where
	toQueryParam (ArtistIds ids) = T.intercalate "," ids

newtype Artists = Artists { unArtists :: [ArtistS] } deriving (Show)
instance FromJSON Artists where
	parseJSON = withObject "Artists" $ \o -> Artists <$> o.: "artists"

data ArtistS = ArtistS {
	artistSExternalUrls :: M.Map T.Text T.Text,
	artistSHref :: T.Text,
	artistSId :: T.Text,
	artistSName :: T.Text,
	artistSURI :: T.Text
} deriving (Show)
instance FromJSON ArtistS where
	parseJSON = withObject "AristS" $ \o -> ArtistS
		<$> o .: "external_urls"
		<*> o .: "href"
		<*> o .: "id"
		<*> o .: "name"
		<*> o .: "uri"

newtype SnapshotResp = SnapshotResp { snapshotRespId :: T.Text } deriving (Show)
instance FromJSON SnapshotResp where
	parseJSON = withObject "SnapshotResp" $ \o -> SnapshotResp <$> o .: "snapshot_id"

data AddTracks = AddTracks {
	addTracksURIs :: [T.Text],
	addTracksPosition :: Maybe Int
} deriving (Show)
instance ToJSON AddTracks where
	toJSON (AddTracks {..}) = object ["uris" .= addTracksURIs, "position" .= addTracksPosition]
	toEncoding (AddTracks {..}) = pairs ("uris" .= addTracksURIs <> "position" .= addTracksPosition)

data RemoveTrack = RemoveTrack {
	removeTrackURI :: T.Text,
	removeTrackPositions :: Maybe [Int]
} deriving (Show)
instance ToJSON RemoveTrack where
	toJSON (RemoveTrack {..}) = object ["uri" .= removeTrackURI, "positions" .= removeTrackPositions]
	toEncoding (RemoveTrack {..}) = pairs ("uri" .= removeTrackURI <> "positions" .= removeTrackPositions)

data RemoveTracks = RemoveTracks {
	removeTracksTracks :: [RemoveTrack],
	removeTracksSnapshotId :: Maybe T.Text
} deriving (Show)
instance ToJSON RemoveTracks where
	toJSON (RemoveTracks {..}) = object ["tracks" .= removeTracksTracks, "snapshot_id" .= removeTracksSnapshotId]
	toEncoding (RemoveTracks {..}) = pairs ("tracks" .= removeTracksTracks <> "snapshot_id" .= removeTracksSnapshotId)

data ReorderTracks = ReorderTracks {
	reorderTracksStart :: Int,
	reorderTracksLength :: Maybe Int,
	reorderTracksBefore :: Int,
	reorderTracksSnapshotId :: Maybe T.Text
} deriving (Show)
instance ToJSON ReorderTracks where
	toJSON (ReorderTracks {..}) = object [
			"range_start" .= reorderTracksStart,
			"range_length" .= reorderTracksLength,
			"insert_before" .= reorderTracksBefore,
			"snapshot_id" .= reorderTracksSnapshotId
		]
	toEncoding (ReorderTracks {..}) = pairs (
			"range_start" .= reorderTracksStart <>
			"range_length" .= reorderTracksLength <>
			"insert_before" .= reorderTracksBefore <>
			"snapshot_id" .= reorderTracksSnapshotId
		)

newtype ReplaceTracks = ReplaceTracks {
	replaceTracksURIs :: [T.Text]
} deriving (Show)
instance ToJSON ReplaceTracks where
	toJSON (ReplaceTracks {..}) = object ["uris" .= replaceTracksURIs]
	toEncoding (ReplaceTracks {..}) = pairs ("uris" .= replaceTracksURIs)

data ChangePlaylistDetails = ChangePlaylistDetails {
	changePlaylistDetailsName :: Maybe T.Text,
	changePlaylistDetailsPublic :: Maybe Bool,
	changePlaylistDetailsCollaborative :: Maybe Bool,
	changePlaylistDetailsDescription :: Maybe T.Text
} deriving (Show)
instance ToJSON ChangePlaylistDetails where
	toJSON (ChangePlaylistDetails {..}) = object [
			"name" .= changePlaylistDetailsName,
			"public" .= changePlaylistDetailsPublic,
			"collaborative" .= changePlaylistDetailsCollaborative,
			"description" .= changePlaylistDetailsDescription
		]
	toEncoding (ChangePlaylistDetails {..}) = pairs (
			"name" .= changePlaylistDetailsName <>
			"public" .= changePlaylistDetailsPublic <>
			"collaborative" .= changePlaylistDetailsCollaborative <>
			"description" .= changePlaylistDetailsDescription
		)

data CreatePlaylist = CreatePlaylist {
	createPlaylistsName :: T.Text,
	createPlaylistsPublic :: Maybe Bool,
	createPlaylistsCollaborative :: Maybe Bool,
	createPlaylistsDescription :: Maybe T.Text
} deriving (Show)
instance ToJSON CreatePlaylist where
	toJSON (CreatePlaylist {..}) = object [
			"name" .= createPlaylistsName,
			"public" .= createPlaylistsPublic,
			"collaborative" .= createPlaylistsCollaborative,
			"description" .= createPlaylistsDescription
		]
	toEncoding (CreatePlaylist {..}) = pairs (
			"name" .= createPlaylistsName <>
			"public" .= createPlaylistsPublic <>
			"collaborative" .= createPlaylistsCollaborative <>
			"description" .= createPlaylistsDescription
		)

data User = User {
	userDisplayName :: Maybe T.Text,
	userExternalURLs :: M.Map T.Text T.Text,
	-- userFollowers :: _,
	userHref :: T.Text,
	userId :: T.Text,
	-- userImages :: [Image],
	userURI :: T.Text
} deriving (Show)
instance FromJSON User where
	parseJSON = withObject "User" $ \o -> User
		<$> o .: "display_name"
		<*> o .: "external_urls"
		-- <*> o .: "followers"
		<*> o .: "href"
		<*> o .: "id"
		-- <*> o .: "images"
		<*> o .: "uri"

data RepeatState = RSOff | RSTrack | RSContext deriving (Show)
instance FromJSON RepeatState where
	parseJSON = withText "RepeatState" $ \case
		"off" -> pure RSOff
		"track" -> pure RSTrack
		"context" -> pure RSContext
		_ -> mempty

data CurrentlyPlayingType = CPTTrack | CPTEpisode | CPTAd | CPTUnknown deriving (Show)
instance FromJSON CurrentlyPlayingType where
	parseJSON = withText "CurrentlyPlayingType" $ \case
		"track" -> pure CPTTrack
		"episode" -> pure CPTEpisode
		"ad" -> pure CPTAd
		"unknown" -> pure CPTUnknown
		_ -> mempty

data CurrentlyPlaying = CurrentlyPlaying {
	-- currentlyPlayingDevice :: Device,
	currentlyPlayingRepeatState :: RepeatState,
	currentlyPlayingShuffleState :: Bool,
	currentlyPlayingContext :: Maybe Context,
	currentlyPlayingTimestamp :: Int,
	currentlyPlayingProgressMS :: Maybe Int,
	currentlyPlayingIsPlaying :: Bool,
	currentlyPlayingItem :: Track,
	currentlyPlayingType :: CurrentlyPlayingType,
	currentlyPlayingActionsDisallows :: M.Map T.Text Bool
} deriving (Show)
instance FromJSON CurrentlyPlaying where
	parseJSON = withObject "CurrentlyPlaying" $ \o -> CurrentlyPlaying
		-- <$> o .: "device"
		<$> o .: "repeat_state"
		<*> o .: "shuffle_state"
		<*> o .: "context"
		<*> o .: "timestamp"
		<*> o .: "progress_ms"
		<*> o .: "is_playing"
		<*> o .: "item"
		<*> o .: "currently_playing_type"
		<*> (o .: "actions" >>= withObject "Actions" (.: "disallows"))

data ContextType = CTAlbum | CTArtist | CTPlaylist deriving (Show)
instance FromJSON ContextType where
	parseJSON = withText "ContextType" $ \case
		"album" -> pure CTAlbum
		"artist" -> pure CTArtist
		"playlist" -> pure CTPlaylist
		_ -> mempty

data Context = Context {
	contextURI :: T.Text,
	contextHref :: T.Text,
	contextExternalURLs :: M.Map T.Text T.Text,
	contextType :: ContextType
} deriving (Show)
instance FromJSON Context where
	parseJSON = withObject "Context" $ \o -> Context
		<$> o .: "uri"
		<*> o .: "href"
		<*> o .: "external_urls"
		<*> o .: "type"

data Offset = OPos Int | OTrack T.Text deriving (Show)
instance ToJSON Offset where
	toJSON (OPos pos) = object ["position" .= pos]
	toJSON (OTrack uri) = object ["uri" .= uri]
	toEncoding (OPos pos) = pairs ("position" .= pos)
	toEncoding (OTrack uri) = pairs ("uri" .= uri)

data PlayTarget = PTResume | PTContext T.Text (Maybe Int) | PTTracks [T.Text] deriving (Show)

data Play = Play {
	playTarget :: PlayTarget,
	playPositionMS :: Maybe Int
} deriving (Show)
instance ToJSON Play where
	toJSON (Play {..}) = object $ join [
			case playTarget of
				PTResume -> []
				PTContext uri offset -> [
						"context_uri" .= uri,
						"offset" .= offset
					]
				PTTracks uris -> ["uris" .= uris]
				,
			["position_ms" .= playPositionMS]
		]

data TrackS = TrackS {
	trackSArtists :: [ArtistS],
	trackSAvailableMarkets :: S.Set T.Text,
	trackSDiscNumber :: Int,
	trackSDurationMS :: Int,
	trackSExplicit :: Bool,
	trackSExternalUrls :: M.Map T.Text T.Text,
	trackSHref :: T.Text,
	trackSId :: T.Text,
	-- trackSIsPlayable :: Bool,
	-- trackSLinkedFrom :: LinkedTrackS,
	trackSName :: T.Text,
	trackSPreviewUrl :: Maybe T.Text,
	trackSNumber :: Int,
	trackSURI :: T.Text
} deriving (Show)
instance FromJSON TrackS where
	parseJSON = withObject "TrackS" $ \o -> TrackS
		<$> o .: "artists"
		<*> o .: "available_markets"
		<*> o .: "disc_number"
		<*> o .: "duration_ms"
		<*> o .: "explicit"
		<*> o .: "external_urls"
		<*> o .: "href"
		<*> o .: "id"
		-- <*> o .: "is_playable"
		-- <*> o .: "linked_from"
		-- <*> o .: "restrictions"
		<*> o .: "name"
		<*> o .: "preview_url"
		<*> o .: "track_number"
		<*> o .: "uri"

data SearchTypeArtist = SearchTypeArtist
instance ToHttpApiData SearchTypeArtist where toQueryParam SearchTypeArtist = "artist"
data SearchTypeAlbum = SearchTypeAlbum
instance ToHttpApiData SearchTypeAlbum where toQueryParam SearchTypeAlbum = "album"
data SearchTypeTrack = SearchTypeTrack
instance ToHttpApiData SearchTypeTrack where toQueryParam SearchTypeTrack = "track"

newtype SearchArtists = SearchArtists { unSearchArtists :: Paging ArtistS }
instance FromJSON SearchArtists where
	parseJSON = withObject "SearchArtists" $ \o -> SearchArtists <$> o.: "artists"
newtype SearchAlbums = SearchAlbums { unSearchAlbums :: Paging Album }
instance FromJSON SearchAlbums where
	parseJSON = withObject "SearchAlbums" $ \o -> SearchAlbums <$> o.: "albums"
newtype SearchTracks = SearchTracks { unSearchTracks :: Paging Track }
instance FromJSON SearchTracks where
	parseJSON = withObject "SearchTracks" $ \o -> SearchTracks <$> o.: "tracks"
