{-|
  WAI (Web Application Interface) Application for static files and CGI.
-}

module Network.Wai.Application.Classic (
  -- * Types
    AppSpec(..)
  , Logger
  , FileInfo(..)
  -- * Files
  , FileRoute(..), fileApp
  -- * CGI
  , CgiRoute(..), cgiApp
  -- * Utilities for logging
  , NumericAddress, showSockAddr
  , FieldKey
  , lookupRequestField, lookupRequestField'
  ) where

import Network.Wai.Application.Classic.CGI
import Network.Wai.Application.Classic.File
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import Network.Wai.Application.Classic.Header
