{-|
  WAI (Web Application Interface) Application for static files and CGI.
-}

module Network.Wai.Application.Classic (
  -- * Files
    FileAppSpec(..)
  , FileInfo(..)
  , FileRoute(..), fileApp
  -- * CGI
  , CgiAppSpec(..)
  , CgiRoute(..), cgiApp
  -- * Reverse Proxy
  , RevProxyAppSpec(..)
  , RevProxyRoute(..), revProxyApp
  ) where

import Network.Wai.Application.Classic.CGI
import Network.Wai.Application.Classic.File
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.RevProxy