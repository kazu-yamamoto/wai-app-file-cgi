{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassicSpec
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import qualified Network.HTTP.Client as H
import Network.Wai
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath
import System.Posix
import Test.Hspec

main :: IO ()
main = do
    void $ installHandler sigCHLD Ignore Nothing
    mgr <- H.newManager H.defaultManagerSettings
    void $ forkIO $ testServer mgr
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
    | cgi = cgiApp appSpec defaultCgiAppSpec cgiRoute req
    | exact = fileApp appSpec defaultFileAppSpec exactRoute req
    | otherwise = fileApp appSpec defaultFileAppSpec fileRoute req
  where
    revproxy = "/revproxy/" `BS.isPrefixOf` rawPathInfo req
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
