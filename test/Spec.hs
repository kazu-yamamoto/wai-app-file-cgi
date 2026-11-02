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
    -- "cabal sdist" drops the executable bit.
    scripts <- listDirectory "test/cgi-bin"
    forM_ scripts $ \f -> do
        let file = "test/cgi-bin" </> f
        p <- getPermissions file
        setPermissions file $ setOwnerExecutable True p
    mgr <- H.newManager H.defaultManagerSettings
    ready1 <- newEmptyMVar
    ready2 <- newEmptyMVar
    void $ forkIO $ testServer (putMVar ready1 ()) mgr
    void $ forkIO $ upstreamServer (putMVar ready2 ())
    takeMVar ready1
    takeMVar ready2
    hspec spec

testServer :: IO () -> H.Manager -> IO ()
testServer ready mgr = do
    dir <- getCurrentDirectory
    runSettings settings $ testApp mgr dir
  where
    settings =
        setBeforeMainLoop ready $
            setPort 2345 $
                setHost "127.0.0.1" defaultSettings

testApp :: H.Manager -> FilePath -> Application
testApp mgr dir req
    | revproxy = revProxyApp appSpec (RevProxyAppSpec mgr) revProxyRoute req
    | upstream = revProxyApp appSpec (RevProxyAppSpec mgr) upstreamRoute req
    | redirect = redirectApp appSpec redirectRoute req
    | cgi = cgiApp appSpec defaultCgiAppSpec cgiRoute req
    | exact = fileApp appSpec defaultFileAppSpec exactRoute req
    | otherwise = fileApp appSpec defaultFileAppSpec fileRoute req
  where
    revproxy = "/revproxy/" `BS.isPrefixOf` rawPathInfo req
    upstream = "/upstream/" `BS.isPrefixOf` rawPathInfo req
    redirect = "/redirect-app/" `BS.isPrefixOf` rawPathInfo req
    cgi = "/cgi-bin/" `BS.isPrefixOf` rawPathInfo req
    exact = "/exact-route.html" `BS.isPrefixOf` rawPathInfo req
    appSpec = defaultClassicAppSpec{softwareName = "ClassicTester"}
    redirectRoute =
        RedirectRoute
            { redirectSrc = "/redirect-app/"
            , redirectDst = "example.com/new/"
            }
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
upstreamServer :: IO () -> IO ()
upstreamServer ready = runSettings settings $ \req respond ->
    respond $ responseLBS ok200 hdr $ BL.unlines $ map toLine $ requestHeaders req
  where
    settings =
        setBeforeMainLoop ready $
            setPort 2346 $
                setHost "127.0.0.1" defaultSettings
    hdr = [("Connection", "X-Up"), ("X-Up", "1"), ("Keep-Alive", "timeout=5")]
    toLine (k, v) = BL.fromStrict $ BS.concat [foldedCase k, ": ", v]
