{-|
  WAI (Web Application Interface) Application for static files and CGI.
-}

module Network.Wai.Application.Classic (
  -- * Types
    AppSpec(..)
  , FileInfo(..)
  -- * Files
  , FileRoute(..), fileApp
  -- * CGI
  , CgiRoute(..), cgiApp
  ) where

import Network.Wai.Application.Classic.CGI
import Network.Wai.Application.Classic.File
import Network.Wai.Application.Classic.Types
