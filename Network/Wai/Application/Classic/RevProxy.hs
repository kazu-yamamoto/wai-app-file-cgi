{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import Control.Applicative
import Control.Exception (SomeException)
import Control.Exception.Lifted as L (catch)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS hiding (uncons)
import qualified Data.ByteString as BS (uncons)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Int
import Data.Maybe
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Conduit
import Network.Wai.Application.Classic.EventSource
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types
import Blaze.ByteString.Builder (Builder)

toHTTPRequest :: Request -> RevProxyRoute -> Int64 -> H.Request (ResourceT IO)
toHTTPRequest req route len = H.def {
    H.host = revProxyDomain route
  , H.port = revProxyPort route
  , H.secure = isSecure req
  , H.requestHeaders = addForwardedFor req $ requestHeaders req
  , H.path = pathByteString path'
  , H.queryString = dropQuestion $ rawQueryString req
  , H.requestBody = getBody req len
  , H.method = requestMethod req
  , H.proxy = Nothing
  , H.rawBody = False
  , H.decompress = H.alwaysDecompress
  , H.checkStatus = \_ _ _ -> Nothing
  , H.redirectCount = 0
  }
  where
    path = fromByteString $ rawPathInfo req
    src = revProxySrc route
    dst = revProxyDst route
    path' = dst </> (path <\> src)
    dropQuestion q = case BS.uncons q of
        Just (63, q') -> q' -- '?' is 63
        _             -> q

getBody :: Request -> Int64 -> H.RequestBody (ResourceT IO)
getBody req len = H.RequestBodySource len (toBodySource req)
  where
    toBodySource r = requestBody r $= CL.map byteStringToBuilder

getLen :: Request -> Maybe Int64
getLen req = do
    len' <- lookup hContentLength $ requestHeaders req
    case reads $ BS.unpack len' of
        [] -> Nothing
        (i, _):_ -> Just i

{-|
  Relaying any requests as reverse proxy.
-}

revProxyApp :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp cspec spec route req =
    revProxyApp' cspec spec route req
    `L.catch` badGateway cspec req

revProxyApp' :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp' cspec spec route req = do
    let mlen = getLen req
        len = fromMaybe 0 mlen
        httpReq = toHTTPRequest req route len
    res <- http httpReq mgr
    let status    = H.responseStatus res
        hdr       = fixHeader $ H.responseHeaders res
        rdownbody = H.responseBody res
    liftIO $ logger cspec req status (fromIntegral <$> mlen)
    ResponseSource status hdr <$> toSource (lookup hContentType hdr) rdownbody
  where
    mgr = revProxyManager spec
    fixHeader = deleteTransferEncoding . addVia cspec req . filter p
    p (k,_)
      | k == hContentEncoding = False
      | k == hContentLength   = False
      | otherwise              = True

toSource :: Maybe BS.ByteString
         -> ResumableSource (ResourceT IO) BS.ByteString
         -> (ResourceT IO) (Source (ResourceT IO) (Flush Builder))
toSource (Just "text/event-stream") = toResponseEventSource
toSource _                          = toResponseSource

type Resp = ResourceT IO (H.Response (ResumableSource (ResourceT IO) BS.ByteString))

http :: H.Request (ResourceT IO) -> H.Manager -> Resp
http req mgr = H.http req mgr

badGateway :: ClassicAppSpec -> Request-> SomeException -> ResourceT IO Response
badGateway cspec req _ = do
    liftIO $ logger cspec req st Nothing -- FIXME body length
    return $ ResponseBuilder st hdr bdy
  where
    hdr = addServer cspec textPlainHeader
    bdy = byteStringToBuilder "Bad Gateway\r\n"
    st = badGateway502
