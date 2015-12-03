{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Def where

import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types

-- |
-- Default value for  'ClassicAppSpec'. 'softwareName' is \"Classic\". 'dater' calls 'epochTime' for every request. 'statusFileDir' is \"\/usr\/local\/share\/html\/status\/\".
defaultClassicAppSpec :: ClassicAppSpec
defaultClassicAppSpec = ClassicAppSpec {
    softwareName = "Classic"
  , statusFileDir = "/usr/local/share/html/status/"
  }

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

