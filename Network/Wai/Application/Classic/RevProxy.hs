{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import Blaze.ByteString.Builder (Builder)
import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (uncons)
import qualified Data.ByteString.Char8 as BS hiding (uncons)
import Data.Conduit
import Data.Default.Class
import qualified Network.HTTP.Client as H
import Network.HTTP.Types
import Network.Wai.Conduit
import Network.Wai.Application.Classic.Conduit
import Network.Wai.Application.Classic.EventSource
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types

----------------------------------------------------------------

-- |  Relaying any requests as reverse proxy.

revProxyApp :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp cspec spec route req respond = bracket setup teardown proxy
  where
    setup = H.responseOpen httpClientRequest mgr
    teardown = H.responseClose
    proxy hrsp = do
        let status     = H.responseStatus hrsp
            hdr        = fixHeader $ H.responseHeaders hrsp
            clientBody = H.responseBody hrsp
            ct         = lookup hContentType hdr
            src        = toSource ct clientBody
        logger cspec req status (fromIntegral <$> mlen)
        respond $ responseSource status hdr src

    httpClientRequest = reqToHReq req route
    mgr = revProxyManager spec
    mlen = case requestBodyLength req of
        ChunkedBody     -> Nothing
        KnownLength len -> Just len
    fixHeader = addVia cspec req . filter headerToBeRelay

headerToBeRelay :: Header -> Bool
headerToBeRelay (k,_)
      | k == hTransferEncoding = False
      | k == hAcceptEncoding   = False
      | k == hContentLength    = False
      | k == hContentEncoding  = False -- See H.decompress.
      | otherwise              = True

----------------------------------------------------------------

reqToHReq :: Request -> RevProxyRoute -> H.Request
reqToHReq req route = def {
    H.host           = revProxyDomain route
  , H.port           = revProxyPort route
  , H.secure         = isSecure req
  , H.requestHeaders = addForwardedFor req $ filter headerToBeRelay hdr
  , H.path           = pathByteString path'
  , H.queryString    = dropQuestion query
  , H.requestBody    = bodyToHBody len body
  , H.method         = requestMethod req
  , H.proxy          = Nothing
--  , H.rawBody        = False
  , H.decompress     = const True
  , H.checkStatus    = \_ _ _ -> Nothing -- FIXME
  , H.redirectCount  = 0
  }
  where
    path = fromByteString $ rawPathInfo req
    src = revProxySrc route
    dst = revProxyDst route
    hdr = requestHeaders req
    query = rawQueryString req
    len = requestBodyLength req
    body = requestBody req
    path' = dst </> (path <\> src)
    dropQuestion q = case BS.uncons q of
        Just (63, q') -> q' -- '?' is 63
        _             -> q

bodyToHBody :: RequestBodyLength -> IO ByteString -> H.RequestBody
bodyToHBody ChunkedBody src       = H.RequestBodyStreamChunked ($ src)
bodyToHBody (KnownLength len) src = H.RequestBodyStream (fromIntegral len) ($ src)

----------------------------------------------------------------

toSource :: Maybe ByteString -> H.BodyReader -> Source IO (Flush Builder)
toSource (Just "text/event-stream") = bodyToEventSource
toSource _                          = bodyToSource

bodyToSource :: H.BodyReader -> Source IO (Flush Builder)
bodyToSource br = loop
  where
    loop = do
        bs <- liftIO $ H.brRead br
        unless (BS.null bs) $ do
            yield $ Chunk $ byteStringToBuilder bs
            loop
{-

FIXME:
badGateway :: ClassicAppSpec -> Request-> SomeException -> IO Response
badGateway cspec req _ = do
    logger cspec req st Nothing -- FIXME body length
    return $ responseBuilder st hdr bdy
  where
    hdr = addServer cspec textPlainHeader
    bdy = byteStringToBuilder "Bad Gateway\r\n"
    st = badGateway502
-}
