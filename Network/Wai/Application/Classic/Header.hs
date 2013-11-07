{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Header where

import Data.Array
import Data.Array.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (tail,break)
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

-- | Lookup key for Transfer-Encoding.
hTransferEncoding :: HeaderName
hTransferEncoding = "transfer-encoding"

----------------------------------------------------------------

hostPort :: Request -> (ByteString, ByteString)
hostPort req = case requestHeaderHost req of
    Nothing -> ("Unknown","80")
    Just hostport -> case BS.break (== ':') hostport of
        (host,"")   -> (host,"80")
        (host,port) -> (host, BS.tail port)

----------------------------------------------------------------

-- | Array for a set of HTTP headers.
type IndexedHeader = Array Int (Maybe ByteString)

----------------------------------------------------------------

indexRequestHeader :: RequestHeaders -> IndexedHeader
indexRequestHeader hdr = traverseHeader hdr requestMaxIndex requestKeyIndex

idxAcceptLanguage,idxIfModifiedSince,idxIfUnmodifiedSince :: Int
idxIfRange,idxRange :: Int
idxAcceptLanguage    = 0
idxIfModifiedSince   = 1
idxIfUnmodifiedSince = 2
idxIfRange           = 3
idxRange             = 4

requestMaxIndex :: Int
requestMaxIndex    = 4

requestKeyIndex :: HeaderName -> Int
requestKeyIndex "accept-language"     = idxAcceptLanguage
requestKeyIndex "if-modified-since"   = idxIfModifiedSince
requestKeyIndex "if-unmodified-since" = idxIfUnmodifiedSince
requestKeyIndex "if-range"            = idxIfRange
requestKeyIndex "range"               = idxRange
requestKeyIndex _                     = -1

defaultIndexRequestHeader :: IndexedHeader
defaultIndexRequestHeader = array (0,requestMaxIndex) [(i,Nothing)|i<-[0..requestMaxIndex]]

----------------------------------------------------------------

traverseHeader :: [Header] -> Int -> (HeaderName -> Int) -> IndexedHeader
traverseHeader hdr maxidx getIndex = runSTArray $ do
    arr <- newArray (0,maxidx) Nothing
    mapM_ (insert arr) hdr
    return arr
  where
    insert arr (key,val)
      | idx == -1 = return ()
      | otherwise = writeArray arr idx (Just val)
      where
        idx = getIndex key
