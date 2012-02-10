{-|
  WAI (Web Application Interface) Application for static files and CGI.
-}

module Network.Wai.Application.Classic (
  -- * Common
    ClassicAppSpec(..)
  , StatusInfo(..)
  -- * Files
  , FileAppSpec(..)
  , FileInfo(..)
  , FileRoute(..)
  , fileApp
  -- * CGI
  , CgiAppSpec(..)
  , CgiRoute(..)
  , cgiApp
  -- * Reverse Proxy
  , RevProxyAppSpec(..)
  , RevProxyRoute(..)
  , revProxyApp
  -- * Path
  , module Network.Wai.Application.Classic.Path
  -- * Misc
  , redirectHeader
  ) where

import Network.Wai.Application.Classic.CGI
import Network.Wai.Application.Classic.File
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.RevProxy
import Network.Wai.Application.Classic.Types
