{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Field where

import Control.Arrow (first)
import Control.Monad (mplus)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (pack)
import Data.ByteString.Char8 as BS (pack)
import qualified Data.Map as Map (toList)
import Data.Maybe
import Data.StaticHash (StaticHash)
import qualified Data.StaticHash as SH
import Network.HTTP.Date
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Lang
import Network.Wai.Application.Static (defaultMimeTypes, defaultMimeType, MimeType, fromFilePath)

----------------------------------------------------------------

languages :: Request -> [Ascii]
languages req = maybe [] parseLang $ lookupRequestField fkAcceptLanguage req

ifModifiedSince :: Request -> Maybe HTTPDate
ifModifiedSince = lookupAndParseDate fkIfModifiedSince

ifUnmodifiedSince :: Request -> Maybe HTTPDate
ifUnmodifiedSince = lookupAndParseDate fkIfUnmodifiedSince

ifRange :: Request -> Maybe HTTPDate
ifRange = lookupAndParseDate fkIfRange

lookupAndParseDate :: FieldKey -> Request -> Maybe HTTPDate
lookupAndParseDate key req = lookupRequestField key req >>= parseHTTPDate

----------------------------------------------------------------

textPlain :: ResponseHeaders
textPlain = [("Content-Type", "text/plain")]

newHeader :: Bool -> ByteString -> HTTPDate -> ResponseHeaders
newHeader ishtml file mtime = [
    ("Content-Type", if ishtml then "text/html" else mimeType file)
  , ("Last-Modified", formatHTTPDate mtime)
  ]

mimeType :: ByteString -> MimeType
mimeType file =fromMaybe defaultMimeType . foldr1 mplus . map lok $ targets
  where
    targets = extensions file
    lok x = SH.lookup x defaultMimeTypes'

extensions :: ByteString -> [ByteString]
extensions file = exts
  where
    entire = case BS.breakByte 46 file of -- '.'
        (_,"") -> ""
        (_,x)  -> BS.tail x
    exts = if entire == "" then [] else entire : BS.split 46 file

defaultMimeTypes' :: StaticHash ByteString MimeType
defaultMimeTypes' = SH.fromList $ map (first (BS.pack.fromFilePath)) $ Map.toList defaultMimeTypes
