{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassicSpec
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Network.HTTP.Date
import Network.Wai
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import System.Directory
import System.FilePath
import System.Log.FastLogger.Date
import System.Posix
import Test.Hspec

main :: IO ()
main = do
    void $ forkIO testServer
    threadDelay 100000
    hspec spec

testServer :: IO ()
testServer = do
    dir <- getCurrentDirectory
    run 2345 $ testApp dir

testApp :: FilePath -> Application
testApp dir req
    | cgi       = cgiApp  appSpec defaultCgiAppSpec  cgiRoute  req
    | otherwise = fileApp appSpec defaultFileAppSpec fileRoute req
  where
    cgi = "/cgi-bin/" `BS.isPrefixOf` rawPathInfo req
    appSpec = defaultAppSpec { softwareName = "ClassicTester" }
    cgiRoute = CgiRoute {
        cgiSrc = "/cgi-bin/"
      , cgiDst = fromString (dir </> "test/cgi-bin/")
      }
    fileRoute = FileRoute {
        fileSrc = "/"
      , fileDst = fromString (dir </> "test/html/")
      }

defaultAppSpec :: ClassicAppSpec
defaultAppSpec = ClassicAppSpec {
    softwareName = "Classic"
  , logger = defaultLogger
  , dater = defaultDater
  , statusFileDir = "/usr/local/share/html/status"
  }

defaultLogger :: ApacheLogger
defaultLogger _ _ _ = return ()

defaultDater :: IO ZonedDate
defaultDater = formatHTTPDate . epochTimeToHTTPDate <$> epochTime

defaultCgiAppSpec :: CgiAppSpec
defaultCgiAppSpec = CgiAppSpec {
    indexCgi = "index.cgi"
  }

defaultFileAppSpec :: FileAppSpec
defaultFileAppSpec = FileAppSpec {
    indexFile = "index.html"
  , isHTML = defaultIsHTml
  , getFileInfo = defaultGetFileInfo
  }

defaultIsHTml :: Path -> Bool
defaultIsHTml file = ".html" `isSuffixOf` file || ".htm" `isSuffixOf` file

defaultGetFileInfo :: Path -> IO FileInfo
defaultGetFileInfo path = do
    fs <- getFileStatus sfile
    if not (isDirectory fs) then
        return FileInfo {
            fileInfoName = path
          , fileInfoSize = size fs
          , fileInfoTime = time fs
          , fileInfoDate = formatHTTPDate (time fs)
          }
      else
        throwIO $ userError "does not exist"
  where
    sfile = pathString path
    size = fromIntegral . fileSize
    time = epochTimeToHTTPDate . modificationTime
