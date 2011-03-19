{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Header where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Maybe
import Network.Wai

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

fkUserAgent :: FieldKey
fkUserAgent = "user-agent"

fkReferer :: FieldKey
fkReferer = "referer"

----------------------------------------------------------------

lookupRequestField :: FieldKey -> Request -> Maybe ByteString
lookupRequestField x req = lookupField x hdrs
  where
    hdrs = requestHeaders req

lookupRequestField' :: FieldKey -> Request -> ByteString
lookupRequestField' x req = fromMaybe "" $ lookupField x hdrs
  where
    hdrs = requestHeaders req

lookupField :: FieldKey -> RequestHeaders -> Maybe ByteString
lookupField x (((CIByteString _ l), val):kvs)
  | x == l       = Just val
  | otherwise    = lookupField x kvs
lookupField _ [] = Nothing
