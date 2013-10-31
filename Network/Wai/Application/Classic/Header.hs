{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Header where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (tail,break)
import Data.Maybe
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

-- | Look-up key for Via.
hVia :: HeaderName
hVia = "via"

-- | Look-up key for Host.
hHost :: HeaderName
hHost = "host"

----------------------------------------------------------------

{-|
  Looking up a header in 'Request'.
-}
lookupRequestField :: HeaderName -> Request -> Maybe ByteString
lookupRequestField x req = lookup x hdrs
  where
    hdrs = requestHeaders req

{-|
  Looking up a header in 'Request'. If the header does not exist,
  empty 'Ascii' is returned.
-}
lookupRequestField' :: HeaderName -> Request -> ByteString
lookupRequestField' x req = fromMaybe "" $ lookup x hdrs
  where
    hdrs = requestHeaders req

----------------------------------------------------------------

hostPort :: RequestHeaders -> (ByteString, ByteString)
hostPort hdrs = case lookup hHost hdrs of
    Nothing -> ("Unknown","80")
    Just hostport -> case BS.break (== ':') hostport of
        (host,"")   -> (host,"80")
        (host,port) -> (host, BS.tail port)
