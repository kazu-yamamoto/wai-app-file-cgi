{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Header where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (
    break,
    drop,
    isPrefixOf,
    null,
    take,
    uncons,
 )
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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.Wai.Internal (Request (..))
-- >>> mkReq h = defaultRequest { requestHeaderHost = h }

-- |
--   Extracting host and port from the Host: header.
--
-- >>> hostPort (mkReq (Just "example.com:8080"))
-- ("example.com","8080")
-- >>> hostPort (mkReq (Just "example.com"))
-- ("example.com","80")
-- >>> hostPort (mkReq (Just "[::1]:8080"))
-- ("[::1]","8080")
-- >>> hostPort (mkReq (Just "[::1]"))
-- ("[::1]","80")
-- >>> hostPort (mkReq (Just "example.com")) { isSecure = True }
-- ("example.com","443")
hostPort :: Request -> (ByteString, ByteString)
hostPort req = case requestHeaderHost req of
    Nothing -> ("Unknown", defaultPort)
    Just hostport -> (host, port)
      where
        (host, rest)
            | "[" `BS.isPrefixOf` hostport =
                let (h, r) = BS.break (== ']') hostport
                 in (h <> BS.take 1 r, BS.drop 1 r)
            | otherwise = BS.break (== ':') hostport
        port = case BS.uncons rest of
            Just (':', p) | not (BS.null p) -> p
            _ -> defaultPort
  where
    defaultPort
        | isSecure req = "443"
        | otherwise = "80"
