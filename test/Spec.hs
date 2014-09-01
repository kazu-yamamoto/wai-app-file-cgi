{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassicSpec
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import Network.Wai
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath
import Test.Hspec
import System.Posix

main :: IO ()
main = do
    void $ installHandler sigCHLD Ignore Nothing
    void $ forkIO testServer
    threadDelay 100000
    hspec spec

testServer :: IO ()
testServer = do
    dir <- getCurrentDirectory
    runSettings settings $ testApp dir
  where
    settings = setPort 2345 $ setHost "127.0.0.1" defaultSettings

testApp :: FilePath -> Application
testApp dir req
    | cgi       = cgiApp  appSpec defaultCgiAppSpec  cgiRoute  req
    | otherwise = fileApp appSpec defaultFileAppSpec fileRoute req
  where
    cgi = "/cgi-bin/" `BS.isPrefixOf` rawPathInfo req
    appSpec = defaultClassicAppSpec { softwareName = "ClassicTester" }
    cgiRoute = CgiRoute {
        cgiSrc = "/cgi-bin/"
      , cgiDst = fromString (dir </> "test/cgi-bin/")
      }
    fileRoute = FileRoute {
        fileSrc = "/"
      , fileDst = fromString (dir </> "test/html/")
      }
