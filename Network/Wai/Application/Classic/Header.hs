{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Header where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.CaseInsensitive
import Data.Maybe
import Network.HTTP.Types
import Network.Wai

----------------------------------------------------------------

-- | Header field key. This must be lower case.
type FieldKey = ByteString

-- | A type for look-up key.
fkAcceptLanguage :: ByteString
fkAcceptLanguage = "accept-language"

-- | Look-up key for Range:.
fkRange :: FieldKey
fkRange = "range"

-- | Look-up key for If-Range:.
fkIfRange :: FieldKey
fkIfRange = "if-range"

-- | Look-up key for Last-Modified:.
fkLastModified :: FieldKey
fkLastModified = "last-modified"

-- | Look-up key for If-Modified-Since:.
fkIfModifiedSince :: FieldKey
fkIfModifiedSince = "if-modified-since"

-- | Look-up key for If-Unmodified-Since:.
fkIfUnmodifiedSince :: FieldKey
fkIfUnmodifiedSince = "if-unmodified-since"

-- | Look-up key for Content-Length:.
fkContentLength :: FieldKey
fkContentLength = "content-length"

-- | Look-up key for Content-Type:.
fkContentType :: FieldKey
fkContentType = "content-type"

-- | Look-up key for Cookie:.
fkCookie :: FieldKey
fkCookie = "cookie"

-- | Look-up key for User-Agent:.
fkUserAgent :: FieldKey
fkUserAgent = "user-agent"

-- | Look-up key for Referer:.
fkReferer :: FieldKey
fkReferer = "referer"

----------------------------------------------------------------

{-|
  Looking up a header in 'Request'.
-}
lookupRequestField :: FieldKey -> Request -> Maybe ByteString
lookupRequestField x req = lookupField x hdrs
  where
    hdrs = requestHeaders req

{-|
  Looking up a header in 'Request'. If the header does not exist,
  empty 'ByteString' is returned.
-}
lookupRequestField' :: FieldKey -> Request -> ByteString
lookupRequestField' x req = fromMaybe "" $ lookupField x hdrs
  where
    hdrs = requestHeaders req

{-|
  Looking up a header in 'RequestHeaders'.
-}
lookupField :: FieldKey -> RequestHeaders -> Maybe ByteString
lookupField x ((key, val):kvs)
  | x == foldedCase key = Just val
  | otherwise           = lookupField x kvs
lookupField _ []        = Nothing
