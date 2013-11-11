{-# LANGUAGE CPP #-}

{-|
  WAI (Web Application Interface) Application for static files and CGI.
-}

module Network.Wai.Application.Classic (
  -- * Common
    ClassicAppSpec(..)
  , defaultClassicAppSpec
  , StatusInfo(..)
  -- * Files
  , FileAppSpec(..)
  , defaultFileAppSpec
  , FileInfo(..)
  , FileRoute(..)
  , fileApp
  -- * Redirect
  , RedirectRoute(..)
  , redirectApp
  -- * CGI
  , CgiAppSpec(..)
  , defaultCgiAppSpec
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
  , hostPort
  ) where

import Network.Wai.Application.Classic.CGI
import Network.Wai.Application.Classic.Def
import Network.Wai.Application.Classic.File
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Redirect
import Network.Wai.Application.Classic.RevProxy
import Network.Wai.Application.Classic.Types
