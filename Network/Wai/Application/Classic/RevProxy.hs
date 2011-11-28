{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB (fromByteString)
import Control.Exception
import Data.ByteString (ByteString)
import Data.Enumerator (Enumerator, Iteratee, run_, (=$), ($$), ($=), enumList)
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

toHTTPRequest :: Request -> RevProxyRoute -> (Enumerator Builder m ())-> H.Request m
toHTTPRequest req route inp = H.def {
    H.host = revProxyDomain route
  , H.port = revProxyPort route
  , H.secure = isSecure req
  , H.checkCerts = H.defaultCheckCerts
  , H.requestHeaders = requestHeaders req
  , H.path = pathByteString path'
  , H.queryString = queryString req
  , H.requestBody = H.RequestBodyEnumChunked inp
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
    bss <- run_ EL.consume
    let inp = enumList 1 bss $= EL.map BB.fromByteString
    run_ (H.http (toHTTPRequest req route inp) (fromBS cspec respBuilder) mgr)
    `catch` badGateway cspec respBuilder
  where
    mgr = revProxyManager spec

fromBS :: ClassicAppSpec
       -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
       -> (Status -> ResponseHeaders -> Iteratee ByteString IO a)
fromBS cspec f s h = EL.map BB.fromByteString -- body: from BS to Builder
            =$ f s h'                -- hedr: removing CE:
  where
    h' = addRevProxyServer cspec $ filter p h
    p ("Content-Encoding", _) = False
    p _ = True

badGateway :: ClassicAppSpec
           -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
           -> SomeException -> IO a
badGateway cspec builder _ = run_ $ bdy $$ builder status502 hdr
  where
    hdr = addRevProxyServer cspec textPlainHeader
    bdy = enumList 1 ["Bad Gateway\r\n"] $= EL.map BB.fromByteString
