{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB (fromByteString)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Enumerator (Iteratee, Enumeratee, run_, (=$), ($$), enumList)
import qualified Data.Enumerator.List as EL
import qualified Network.HTTP.Enumerator as H
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import Prelude hiding (catch)

toHTTPRequest :: Request -> RevProxyRoute -> BL.ByteString -> H.Request m
toHTTPRequest req route lbs = H.def {
    H.host = revProxyDomain route
  , H.port = revProxyPort route
  , H.secure = isSecure req
  , H.checkCerts = H.defaultCheckCerts
  , H.requestHeaders = addForwardedFor req $ requestHeaders req
  , H.path = pathByteString path'
  , H.queryString = queryString req
  , H.requestBody = H.RequestBodyLBS lbs
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

{-|
  Relaying any requests as reverse proxy.
-}

revProxyApp :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp cspec spec route req = respEnumerator $ \respIter -> do
    -- FIXME: stored-and-forward -> streaming
    lbs <- BL.fromChunks <$> run_ EL.consume
    run_ (H.http (toHTTPRequest req route lbs) (fromBS cspec req respIter) mgr)
      `catch` badGateway cspec req respIter
  where
    respEnumerator = return . ResponseEnumerator
    mgr = revProxyManager spec

fromBS :: ClassicAppSpec -> Request
       -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
       -> (Status -> ResponseHeaders -> Iteratee ByteString IO a)
fromBS cspec req respIter st hdr = do
    liftIO $ logger cspec req st Nothing -- FIXME body length
    bodyAsBuilder =$ respIter st hdr'
  where
    hdr' = addVia cspec req $ filter p hdr
    p ("Content-Encoding", _) = False
    p _ = True

badGateway :: ClassicAppSpec -> Request
           -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
           -> SomeException -> IO a
badGateway cspec req respIter _ = do
    liftIO $ logger cspec req st Nothing -- FIXME body length
    run_ $ bdy $$ bodyAsBuilder =$ respIter st hdr
  where
    hdr = addServer cspec textPlainHeader
    bdy = enumList 1 ["Bad Gateway\r\n"]
    st = statusBadGateway

bodyAsBuilder :: Enumeratee ByteString Builder IO a
bodyAsBuilder = EL.map BB.fromByteString
