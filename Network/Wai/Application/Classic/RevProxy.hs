{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import Blaze.ByteString.Builder (Builder, fromByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Enumerator (Iteratee, run_, (=$))
import qualified Data.Enumerator.List as EL
import qualified Network.HTTP.Enumerator as H
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils

{- TODO
 - incremental boy (persist connection)
 - 202 when no target
-}

toHTTPRequest :: Request -> RevProxyRoute -> H.Request m
toHTTPRequest req route = H.def {
    H.host = revProxyDomain route
  , H.port = revProxyPort route
  , H.secure = isSecure req
  , H.checkCerts = H.defaultCheckCerts
  , H.requestHeaders = []
  , H.path = path
  , H.queryString = queryString req
  , H.requestBody = H.RequestBodyLBS L.empty -- xxx Ah Ha!
  , H.method = requestMethod req
  , H.proxy = Nothing
  , H.rawBody = False
  , H.decompress = H.alwaysDecompress
  }
  where
    src = revProxySrc route
    dst = revProxyDst route
    path = dst +++ BS.drop (BS.length src) (rawPathInfo req)

revProxyApp :: RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp spec route req = return $ ResponseEnumerator $ \buildHeader ->
    run_ (H.http (toHTTPRequest req route) (blaze buildHeader) mgr)
  where
    mgr = revProxyManager spec

blaze :: (Status -> ResponseHeaders -> Iteratee Builder IO a)
      -> (Status -> ResponseHeaders -> Iteratee ByteString IO a)
blaze f s h = EL.map fromByteString =$ f s h'
  where
    h' = filter p h
    p ("Content-Encoding", _) = False
    p _ = True
