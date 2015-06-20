{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Field where

import Control.Arrow (first)
import Control.Monad (mplus)
import Data.Array ((!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import Data.Maybe
import Data.StaticHash (StaticHash)
import qualified Data.StaticHash as SH
import qualified Data.Text as T
import Network.HTTP.Date
import Network.HTTP.Types
import Network.Mime (defaultMimeMap, defaultMimeType, MimeType)
import Network.SockAddr
import Network.Wai
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Lang
import Network.Wai.Application.Classic.Types

----------------------------------------------------------------

languages :: IndexedHeader -> [ByteString]
languages reqidx = maybe [] parseLang $ reqidx ! idxAcceptLanguage

ifModifiedSince :: IndexedHeader -> Maybe HTTPDate
ifModifiedSince reqidx = reqidx ! idxIfModifiedSince >>= parseHTTPDate

ifUnmodifiedSince :: IndexedHeader -> Maybe HTTPDate
ifUnmodifiedSince reqidx = reqidx ! idxIfUnmodifiedSince >>= parseHTTPDate

ifRange :: IndexedHeader -> Maybe HTTPDate
ifRange reqidx = reqidx ! idxIfRange >>= parseHTTPDate

----------------------------------------------------------------

textPlainHeader :: ResponseHeaders
textPlainHeader = [(hContentType,"text/plain")]

textHtmlHeader :: ResponseHeaders
textHtmlHeader = [(hContentType,"text/html")]

locationHeader :: ByteString -> ResponseHeaders
locationHeader url = [(hLocation, url)]

-- FIXME: the case where "Via:" already exists
addVia :: ClassicAppSpec -> Request -> ResponseHeaders -> ResponseHeaders
addVia cspec req hdr = (hVia, val) : hdr
  where
    ver = httpVersion req
    val = BS.concat [
        showBS (httpMajor ver)
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
addForwardedFor req hdr = (hXForwardedFor, addr) : hdr
  where
    addr = B8.pack . showSockAddr . remoteHost $ req

newHeader :: Bool -> ByteString -> ByteString -> ResponseHeaders
newHeader ishtml file date
  | ishtml    = lastMod : textHtmlHeader
  | otherwise = lastMod : (hContentType, mimeType file) : []
  where
    lastMod = (hLastModified, date)

mimeType :: ByteString -> MimeType
mimeType file =fromMaybe defaultMimeType . foldr1 mplus . map lok $ targets
  where
    targets = extensions file
    lok x = SH.lookup x defaultMimeTypes'

extensions :: ByteString -> [ByteString]
extensions file = exts
  where
    entire = case BS.break (== 46) file of -- '.'
        (_,"") -> ""
        (_,x)  -> BS.tail x
    exts = if entire == "" then [] else entire : BS.split 46 file

defaultMimeTypes' :: StaticHash ByteString MimeType
defaultMimeTypes' = SH.fromList $ map (first (B8.pack . T.unpack)) $ Map.toList defaultMimeMap

showBS :: Show a => a -> ByteString
showBS = B8.pack . show
