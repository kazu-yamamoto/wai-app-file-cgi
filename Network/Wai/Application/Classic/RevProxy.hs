{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.RevProxy (revProxyApp) where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (uncons)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as BS hiding (uncons)
import Data.CaseInsensitive (mk)
import Data.Conduit
import Data.IORef
import qualified Network.HTTP.Client as H
import Network.HTTP.Types
import Network.Wai.Application.Classic.Conduit
import Network.Wai.Application.Classic.EventSource
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types
import Network.Wai.Conduit

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

-- |  Relaying any requests as reverse proxy.
revProxyApp :: ClassicAppSpec -> RevProxyAppSpec -> RevProxyRoute -> Application
revProxyApp cspec spec route req respond = do
    responded <- newIORef False
    H.withResponse httpClientRequest mgr (proxy responded)
        `E.catch` badGateway responded
  where
    proxy responded hrsp = do
        let status = H.responseStatus hrsp
            hdr = fixHeader $ H.responseHeaders hrsp
            clientBody = H.responseBody hrsp
            ct = lookup hContentType hdr
            src = toSource ct clientBody
        writeIORef responded True
        respond $ responseSource status hdr src

    -- The response cannot be replaced once it has been started.
    badGateway responded e = do
        started <- readIORef responded
        if started
            then E.throwIO (e :: H.HttpException)
            else respond $ responseLBS badGateway502 textPlainHeader "Bad Gateway\r\n"

    httpClientRequest = reqToHReq cspec req route
    mgr = revProxyManager spec
    fixHeader = addVia cspec req . filterHeaders

-- | Removing hop-by-hop headers and ones handled by http-client.
--
-- >>> filterHeaders [("Connection","close, X-Foo"),("X-Foo","1"),("Keep-Alive","5"),("X-Bar","2")]
-- [("X-Bar","2")]
filterHeaders :: [Header] -> [Header]
filterHeaders hdr = filter (\(k, _) -> k `notElem` excluded) hdr
  where
    excluded = connectionTokens ++ hopByHop
    connectionTokens =
        [ mk $ BS.dropWhile (== ' ') $ BS.dropWhileEnd (== ' ') t
        | (k, v) <- hdr
        , k == hConnection
        , t <- BS.split ',' v
        ]
    hopByHop =
        [ hConnection
        , "keep-alive"
        , "proxy-connection"
        , hProxyAuthenticate
        , hProxyAuthorization
        , hTE
        , "trailer"
        , hTransferEncoding
        , hUpgrade
        , hAcceptEncoding
        , hContentLength
        , hContentEncoding -- See H.decompress.
        ]

----------------------------------------------------------------

reqToHReq :: ClassicAppSpec -> Request -> RevProxyRoute -> H.Request
reqToHReq cspec req route =
    H.defaultRequest
        { H.host = revProxyDomain route
        , H.port = revProxyPort route
        , H.secure = False -- FIXME: upstream is HTTP only
        , H.requestHeaders = addVia cspec req $ addForwardedFor req $ filterHeaders hdr
        , H.path = path'
        , H.queryString = dropQuestion query
        , H.requestBody = bodyToHBody len body
        , H.method = requestMethod req
        , H.proxy = Nothing
        , --  , H.rawBody        = False
          H.decompress = const True
        , H.checkResponse = \_ _ -> return ()
        , H.redirectCount = 0
        }
  where
    path = rawPathInfo req
    src = revProxySrc route
    dst = revProxyDst route
    hdr = requestHeaders req
    query = rawQueryString req
    len = requestBodyLength req
    body = getRequestBodyChunk req
    path' = dst </> (path <\> src)
    dropQuestion q = case BS.uncons q of
        Just (63, q') -> q' -- '?' is 63
        _ -> q

bodyToHBody :: RequestBodyLength -> IO ByteString -> H.RequestBody
bodyToHBody ChunkedBody src = H.RequestBodyStreamChunked ($ src)
bodyToHBody (KnownLength len) src = H.RequestBodyStream (fromIntegral len) ($ src)

----------------------------------------------------------------

toSource :: Maybe ByteString -> H.BodyReader -> ConduitT () (Flush Builder) IO ()
toSource (Just "text/event-stream") = bodyToEventSource
toSource _ = bodyToSource

bodyToSource :: H.BodyReader -> ConduitT () (Flush Builder) IO ()
bodyToSource br = loop
  where
    loop = do
        bs <- liftIO $ H.brRead br
        unless (BS.null bs) $ do
            yield $ Chunk $ byteStringToBuilder bs
            loop
