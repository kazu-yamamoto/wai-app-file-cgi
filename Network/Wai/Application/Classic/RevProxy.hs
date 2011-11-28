{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB (fromByteString)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Enumerator (Iteratee, run_, (=$), ($$), ($=), enumList)
import qualified Data.Enumerator.List as EL
import qualified Network.HTTP.Enumerator as H
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import Prelude hiding (catch)

{- TODO
 - incremental boy (persist connection)
 - Body
-}

toHTTPRequest :: Request -> RevProxyRoute -> BL.ByteString -> H.Request m
toHTTPRequest req route lbs = H.def {
    H.host = revProxyDomain route
  , H.port = revProxyPort route
  , H.secure = isSecure req
  , H.checkCerts = H.defaultCheckCerts
  , H.requestHeaders = requestHeaders req
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
  Relaying HTTP body is not implemented yet.
-}

revProxyApp :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp cspec spec route req = return $ ResponseEnumerator $ \respBuilder -> do
    -- FIXME: is this stored-and-forward?
    lbs <- BL.fromChunks <$> run_ EL.consume
    run_ (H.http (toHTTPRequest req route lbs) (fromBS cspec req respBuilder) mgr)
    `catch` badGateway cspec req respBuilder
  where
    mgr = revProxyManager spec

fromBS :: ClassicAppSpec -> Request
       -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
       -> (Status -> ResponseHeaders -> Iteratee ByteString IO a)
fromBS cspec req f s h = do
    liftIO $ logger cspec req s Nothing -- FIXME body length
    EL.map BB.fromByteString =$ f s h'
  where
    h' = addForwardedFor req $ addVia cspec req $ filter p h
    p ("Content-Encoding", _) = False
    p _ = True

badGateway :: ClassicAppSpec -> Request
           -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
           -> SomeException -> IO a
badGateway cspec req builder _ = do
    liftIO $ logger cspec req statusBadGateway Nothing -- FIXME body length
    run_ $ bdy $$ builder statusBadGateway hdr
  where
    hdr = addServer cspec textPlainHeader
    bdy = enumList 1 ["Bad Gateway\r\n"] $= EL.map BB.fromByteString
