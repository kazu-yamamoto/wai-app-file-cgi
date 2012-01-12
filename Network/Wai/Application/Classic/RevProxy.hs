{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB (fromByteString)
import Control.Applicative
import Control.Exception (SomeException)
import Control.Exception.Lifted (catch)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Int
import Data.Maybe
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import Prelude hiding (catch)

toHTTPRequest :: Request -> RevProxyRoute -> Int64 -> H.Request IO
toHTTPRequest req route len = H.def {
    H.host = revProxyDomain route
  , H.port = revProxyPort route
  , H.secure = isSecure req
  , H.checkCerts = H.defaultCheckCerts
  , H.requestHeaders = addForwardedFor req $ requestHeaders req
  , H.path = pathByteString path'
  , H.queryString = rawQueryString req
  , H.requestBody = getBody req len
  , H.method = requestMethod req
  , H.proxy = Nothing
  , H.rawBody = False
  , H.decompress = H.alwaysDecompress
  }
  where
    path = fromByteString $ rawPathInfo req
    src = revProxySrc route
    dst = revProxyDst route
    path' = dst </> (path <\> src)

toSource :: BufferedSource IO ByteString -> Source IO Builder
toSource = fmap BB.fromByteString . unbufferSource

getBody :: Request -> Int64 -> H.RequestBody IO
getBody req len = H.RequestBodySource len (toSource . requestBody $ req)

getLen :: Request -> Maybe Int64
getLen req = do
    len' <- lookup "content-length" $ requestHeaders req
    case reads $ BS.unpack len' of
        [] -> Nothing
        (i, _):_ -> Just i

{-|
  Relaying any requests as reverse proxy.
-}

revProxyApp :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp cspec spec route req =
    revProxyApp' cspec spec route req
    `catch` badGateway cspec req

revProxyApp' :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp' cspec spec route req = do
    let mlen = getLen req
        len = fromMaybe 0 mlen
        httpReq = toHTTPRequest req route len
    H.Response status hdr downbody <- H.http httpReq mgr
    let hdr' = fixHeader hdr
    liftIO $ logger cspec req status (fromIntegral <$> mlen)
    return $ ResponseSource status hdr' (toSource downbody)
  where
    mgr = revProxyManager spec
    fixHeader = addVia cspec req . filter p
    p ("Content-Encoding", _) = False
    p _ = True

badGateway :: ClassicAppSpec -> Request-> SomeException -> ResourceT IO Response
badGateway cspec req _ = do
    liftIO $ logger cspec req st Nothing -- FIXME body length
    return $ ResponseBuilder st hdr bdy
  where
    hdr = addServer cspec textPlainHeader
    bdy = BB.fromByteString "Bad Gateway\r\n"
    st = statusBadGateway
