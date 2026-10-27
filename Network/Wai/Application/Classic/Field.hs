{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Field where

import Control.Arrow (first)
import Control.Monad (mplus)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Maybe
import Data.StaticHash (StaticHash)
import qualified Data.StaticHash as SH
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Mime (MimeType, defaultMimeMap, defaultMimeType)
import Network.SockAddr
import Network.Wai
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Lang
import Network.Wai.Application.Classic.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

languages :: RequestHeaders -> [ByteString]
languages = maybe [] parseLang . lookup hAcceptLanguage

----------------------------------------------------------------

textPlainHeader :: ResponseHeaders
textPlainHeader = [(hContentType, "text/plain")]

textHtmlHeader :: ResponseHeaders
textHtmlHeader = [(hContentType, "text/html")]

locationHeader :: ByteString -> ResponseHeaders
locationHeader url = [(hLocation, url)]

addVia :: ClassicAppSpec -> Request -> ResponseHeaders -> ResponseHeaders
addVia cspec req = appendHeader hVia val
  where
    ver = httpVersion req
    val =
        BS.concat
            [ showBS (httpMajor ver)
            , "."
            , showBS (httpMinor ver)
            , " "
            , host
            , " ("
            , softwareName cspec
            , ")"
            ]
    host = fromMaybe "" $ requestHeaderHost req

addForwardedFor :: Request -> ResponseHeaders -> ResponseHeaders
addForwardedFor req = appendHeader hXForwardedFor addr
  where
    addr = B8.pack . showSockAddr . remoteHost $ req

-- | Appending a value to the existing header if any.
--
-- >>> appendHeader "X-Forwarded-For" "192.0.2.2" [("Host","a"),("X-Forwarded-For","192.0.2.1")]
-- [("X-Forwarded-For","192.0.2.1, 192.0.2.2"),("Host","a")]
-- >>> appendHeader "X-Forwarded-For" "192.0.2.2" [("Host","a")]
-- [("X-Forwarded-For","192.0.2.2"),("Host","a")]
appendHeader :: HeaderName -> ByteString -> [Header] -> [Header]
appendHeader key val hdr = case [v | (k, v) <- hdr, k == key] of
    [] -> (key, val) : hdr
    vs -> (key, BS.intercalate ", " (vs ++ [val])) : filter ((/= key) . fst) hdr

newHeader :: Bool -> ByteString -> ResponseHeaders
newHeader ishtml file
    | ishtml = textHtmlHeader
    | otherwise = [(hContentType, mimeType file)]

-- |
-- >>> mimeType "/var/www/a.html.d/pic.png"
-- "image/png"
-- >>> mimeType "/var/www/www.example.org/README"
-- "application/octet-stream"
-- >>> mimeType "/var/www/PIC.PNG"
-- "image/png"
mimeType :: ByteString -> MimeType
mimeType file = fromMaybe defaultMimeType . foldr mplus Nothing . map lok $ targets
  where
    targets = extensions file
    lok x = SH.lookup x defaultMimeTypes'

-- | Extensions of the file name, the longest first.
--
-- >>> extensions "/var/www.d/foo.tar.gz"
-- ["tar.gz","gz"]
-- >>> extensions "/var/www.d/foo"
-- []
extensions :: ByteString -> [ByteString]
extensions file = go name
  where
    name = B8.map toLower $ snd $ BS.breakEnd (== 47) file -- '/'
    go x = case BS.break (== 46) x of -- '.'
        (_, "") -> []
        (_, ext) -> let ext' = BS.tail ext in ext' : go ext'

defaultMimeTypes' :: StaticHash ByteString MimeType
defaultMimeTypes' = SH.fromList $ map (first (B8.pack . T.unpack)) $ Map.toList defaultMimeMap

showBS :: Show a => a -> ByteString
showBS = B8.pack . show
