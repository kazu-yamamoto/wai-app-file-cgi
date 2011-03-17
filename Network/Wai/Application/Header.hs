{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Header where

import Control.Monad (mplus)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.List
import qualified Data.Map as M (lookup)
import Data.Maybe
import Data.Time
import Network.Wai
import Network.Wai.Application.Date
import Network.Wai.Application.Lang
import Network.Wai.Application.Static (defaultMimeTypes, defaultMimeType, MimeType)

----------------------------------------------------------------

type FieldKey = ByteString

fkAcceptLanguage :: ByteString
fkAcceptLanguage = "accept-language"

fkRange :: FieldKey
fkRange = "range"

fkIfRange :: FieldKey
fkIfRange = "if-range"

fkLastModified :: FieldKey
fkLastModified = "last-modified"

fkIfModifiedSince :: FieldKey
fkIfModifiedSince = "if-modified-since"

fkIfUnmodifiedSince :: FieldKey
fkIfUnmodifiedSince = "if-unmodified-since"

fkContentLength :: FieldKey
fkContentLength = "content-length"

fkContentType :: FieldKey
fkContentType = "content-type"

fkCookie :: FieldKey
fkCookie = "cookie"

----------------------------------------------------------------

lookupRequestField :: FieldKey -> Request -> Maybe ByteString
lookupRequestField x req = lookupField x hdrs
  where
    hdrs = requestHeaders req

lookupField :: FieldKey -> RequestHeaders -> Maybe ByteString
lookupField x (((CIByteString _ l), val):kvs)
  | x == l       = Just val
  | otherwise    = lookupField x kvs
lookupField _ [] = Nothing

----------------------------------------------------------------

languages :: Request -> [String]
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

newHeader :: FilePath -> UTCTime -> ResponseHeaders
newHeader file mtime = [
    ("Content-Type", mimeType file)
  , ("Last-Modified", utcToDate mtime)
  ]

mimeType :: FilePath -> MimeType
mimeType file =fromMaybe defaultMimeType . foldl1' mplus . map lok $ targets
  where
    targets = extensions file
    lok x = M.lookup x defaultMimeTypes

extensions :: FilePath -> [String]
extensions file = entire : exts
  where
    exts' = split ('.'==) file
    exts = if exts' == [] then [] else tail exts'
    entire = foldr (\x y -> x ++ '.' : y) "" exts

split :: (Char -> Bool) -> String -> [String]
split _ "" = []
split p xs = case break p xs of
    (ys,"")   -> [ys]
    (ys,_:zs) -> ys : split p zs
