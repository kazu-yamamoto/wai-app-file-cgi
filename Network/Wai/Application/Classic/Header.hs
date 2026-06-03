{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Header where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (break, tail)
import Network.HTTP.Types.Header
import Network.Wai

----------------------------------------------------------------

-- | Look-up key for If-Unmodified-Since:.
hIfUnmodifiedSince :: HeaderName
hIfUnmodifiedSince = "if-unmodified-since"

-- | Look-up key for Status.
hStatus :: HeaderName
hStatus = "status"

-- | Look-up key for X-Forwarded-For.
hXForwardedFor :: HeaderName
hXForwardedFor = "x-forwarded-for"

----------------------------------------------------------------

hostPort :: Request -> (ByteString, ByteString)
hostPort req = case requestHeaderHost req of
    Nothing -> ("Unknown", "80")
    Just hostport -> case BS.break (== ':') hostport of
        (host, "") -> (host, "80")
        (host, port) -> (host, BS.tail port)
