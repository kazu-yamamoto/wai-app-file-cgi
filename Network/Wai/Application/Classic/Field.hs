{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Field where

import Control.Arrow (first)
import Control.Monad (mplus)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (pack)
import Data.ByteString.Char8 as BS (pack)
import Data.HashMap (Map)
import qualified Data.HashMap as M
import Data.List
import Data.Map as Map (toList)
import Data.Maybe
import Data.Time
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Date
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Lang
import Network.Wai.Application.Static (defaultMimeTypes, defaultMimeType, MimeType)

----------------------------------------------------------------

languages :: Request -> [ByteString]
languages req = maybe [] parseLang $ lookupRequestField fkAcceptLanguage req

ifModifiedSince :: Request -> Maybe UTCTime
ifModifiedSince = lookupAndParseDate fkIfModifiedSince

ifUnmodifiedSince :: Request -> Maybe UTCTime
ifUnmodifiedSince = lookupAndParseDate fkIfUnmodifiedSince

ifRange :: Request -> Maybe UTCTime
ifRange = lookupAndParseDate fkIfRange

lookupAndParseDate :: ByteString -> Request -> Maybe UTCTime
lookupAndParseDate key req = lookupRequestField key req >>= parseDate

----------------------------------------------------------------

textPlain :: ResponseHeaders
textPlain = [("Content-Type", "text/plain")]

newHeader :: ByteString -> UTCTime -> ResponseHeaders
newHeader file mtime = [
    ("Content-Type", mimeType file)
  , ("Last-Modified", utcToDate mtime)
  ]

mimeType :: ByteString -> MimeType
mimeType file =fromMaybe defaultMimeType . foldr1 mplus . map lok $ targets
  where
    targets = extensions file
    lok x = M.lookup x defaultMimeTypes'

extensions :: ByteString -> [ByteString]
extensions file = exts
  where
    entire = case BS.breakByte 46 file of -- '.'
        (_,"") -> ""
        (_,x)  -> BS.tail x
    exts = if entire == "" then [] else entire : BS.split 46 file

defaultMimeTypes' :: Map ByteString MimeType
defaultMimeTypes' = M.fromList $ map (first BS.pack) $ Map.toList defaultMimeTypes
