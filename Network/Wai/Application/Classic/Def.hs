{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Def where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types

-- |
-- Default value for  'ClassicAppSpec'. 'softwareName' is \"Classic\". 'logger' does not log at all. 'dater' calls 'epochTime' for every request. 'statusFileDir' is \"\/usr\/local\/share\/html\/status\/\".
defaultClassicAppSpec :: ClassicAppSpec
defaultClassicAppSpec = ClassicAppSpec {
    softwareName = "Classic"
  , logger = defaultLogger
  , statusFileDir = "/usr/local/share/html/status/"
  }

defaultLogger :: Request -> Status -> Maybe Integer -> IO ()
defaultLogger _ _ _ = return ()

----------------------------------------------------------------

-- |
-- Default value for 'defaultFileAppSpec'. 'indexFile' is \"index.html\". 'isHTML' matches \"*.html\" and \"*.html\".
defaultFileAppSpec :: FileAppSpec
defaultFileAppSpec = FileAppSpec {
    indexFile = "index.html"
  , isHTML = defaultIsHTml
  }

defaultIsHTml :: Path -> Bool
defaultIsHTml file = ".html" `isSuffixOf` file || ".htm" `isSuffixOf` file

----------------------------------------------------------------

-- |
-- Default value for 'defaultCgiAppSpec'. 'indexCgi' is \"index.cgi\".
defaultCgiAppSpec :: CgiAppSpec
defaultCgiAppSpec = CgiAppSpec {
    indexCgi = "index.cgi"
  }

