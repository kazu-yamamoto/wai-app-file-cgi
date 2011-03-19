{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Field where

import Control.Monad (mplus)
import Data.List
import qualified Data.Map as M (lookup)
import Data.Time
import Network.Wai.Application.Classic.Date
import Network.Wai.Application.Classic.Lang
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Static (defaultMimeTypes, defaultMimeType, MimeType)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Maybe
import Network.Wai

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
