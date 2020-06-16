module Sepo.DBusClient where

import DBus
import DBus.Client
import qualified Data.Text as T
import Control.Monad.IO.Class

play :: MonadIO m => Client -> T.Text -> m ()
play client uri = liftIO $ callNoReply client $ (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "OpenUri") {
		methodCallDestination = Just "org.mpris.MediaPlayer2.spotify",
		methodCallBody = [toVariant uri]
	}
