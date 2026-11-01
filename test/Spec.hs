{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassicSpec
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.CaseInsensitive (foldedCase)
import qualified Network.HTTP.Client as H
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath
import Test.Hspec

main :: IO ()
main = do
    mgr <- H.newManager H.defaultManagerSettings
    void $ forkIO $ testServer mgr
    void $ forkIO upstreamServer
    threadDelay 100000
    hspec spec

testServer :: H.Manager -> IO ()
testServer mgr = do
    dir <- getCurrentDirectory
    runSettings settings $ testApp mgr dir
  where
    settings = setPort 2345 $ setHost "127.0.0.1" defaultSettings

testApp :: H.Manager -> FilePath -> Application
testApp mgr dir req
    | revproxy = revProxyApp appSpec (RevProxyAppSpec mgr) revProxyRoute req
    | upstream = revProxyApp appSpec (RevProxyAppSpec mgr) upstreamRoute req
    | cgi = cgiApp appSpec defaultCgiAppSpec cgiRoute req
    | exact = fileApp appSpec defaultFileAppSpec exactRoute req
    | otherwise = fileApp appSpec defaultFileAppSpec fileRoute req
  where
    revproxy = "/revproxy/" `BS.isPrefixOf` rawPathInfo req
    upstream = "/upstream/" `BS.isPrefixOf` rawPathInfo req
    cgi = "/cgi-bin/" `BS.isPrefixOf` rawPathInfo req
    exact = "/exact-route.html" `BS.isPrefixOf` rawPathInfo req
    appSpec = defaultClassicAppSpec{softwareName = "ClassicTester"}
    cgiRoute =
        CgiRoute
            { cgiSrc = "/cgi-bin/"
            , cgiDst = fromString (dir </> "test/cgi-bin/")
            }
    exactRoute =
        FileRoute
            { fileSrc = "/exact-route.html"
            , fileDst = fromString (dir </> "test/html/index.html")
            }
    fileRoute =
        FileRoute
            { fileSrc = "/"
            , fileDst = fromString (dir </> "test/html/")
            }
    -- Nothing should be listening on this port.
    revProxyRoute =
        RevProxyRoute
            { revProxySrc = "/revproxy/"
            , revProxyDst = "/"
            , revProxyDomain = "127.0.0.1"
            , revProxyPort = 1
            }
    upstreamRoute =
        RevProxyRoute
            { revProxySrc = "/upstream/"
            , revProxyDst = "/"
            , revProxyDomain = "127.0.0.1"
            , revProxyPort = 2346
            }

-- | Echoing request headers with hop-by-hop response headers.
upstreamServer :: IO ()
upstreamServer = runSettings settings $ \req respond ->
    respond $ responseLBS ok200 hdr $ BL.unlines $ map toLine $ requestHeaders req
  where
    settings = setPort 2346 $ setHost "127.0.0.1" defaultSettings
    hdr = [("Connection", "X-Up"), ("X-Up", "1"), ("Keep-Alive", "timeout=5")]
    toLine (k, v) = BL.fromStrict $ BS.concat [foldedCase k, ": ", v]
