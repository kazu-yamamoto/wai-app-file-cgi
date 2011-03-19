{-|
  WAI (Web Application Interface) Application for static files and CGI.
-}

module Network.Wai.Application.Classic (
    module Network.Wai.Application.Classic.CGI
  , module Network.Wai.Application.Classic.File
  , module Network.Wai.Application.Classic.Types
  , module Network.Wai.Application.Classic.Header
  , module Network.Wai.Application.Classic.Utils
  ) where

import Network.Wai.Application.Classic.CGI
import Network.Wai.Application.Classic.File
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Utils

