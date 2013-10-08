{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Def where

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import Network.HTTP.Date
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types
import System.Posix

-- |
-- Default value for  'ClassicAppSpec'. 'softwareName' is \"Classic\". 'logger' does not log at all. 'dater' calls 'epochTime' for every request. 'statusFileDir' is \"\/usr\/local\/share\/html\/status\/\".
defaultClassicAppSpec :: ClassicAppSpec
defaultClassicAppSpec = ClassicAppSpec {
    softwareName = "Classic"
  , logger = defaultLogger
  , dater = defaultDater
  , statusFileDir = "/usr/local/share/html/status/"
  }

defaultLogger :: Request -> Status -> Maybe Integer -> IO ()
defaultLogger _ _ _ = return ()

defaultDater :: IO ByteString
defaultDater = formatHTTPDate . epochTimeToHTTPDate <$> epochTime

----------------------------------------------------------------

-- |
-- Default value for 'defaultFileAppSpec'. 'indexFile' is \"index.html\". 'isHTML' matches \"*.html\" and \"*.html\". 'getFileInfo' calls `getFileStatus` for every request.
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

----------------------------------------------------------------

-- |
-- Default value for 'defaultCgiAppSpec'. 'indexCgi' is \"index.cgi\".
defaultCgiAppSpec :: CgiAppSpec
defaultCgiAppSpec = CgiAppSpec {
    indexCgi = "index.cgi"
  }

