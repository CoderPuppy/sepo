module Sepo.DBusClient where

import DBus
import DBus.Client
import qualified Data.Text as T

play :: Client -> T.Text -> IO ()
play client uri = callNoReply client $ (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "OpenUri") {
		methodCallDestination = Just "org.mpris.MediaPlayer2.spotify",
		methodCallBody = [toVariant uri]
	}
