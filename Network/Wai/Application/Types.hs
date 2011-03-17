{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Types where

import Data.ByteString (ByteString)
import Network.Wai

data AppSpec = AppSpec {
    softwareName :: String
  , indexFile :: FilePath
  , isHTML :: FilePath -> Bool
  }

data FileRoute = FileRoute {
    fileSrc :: ByteString
  , fileDst :: FilePath
  }

data CgiRoute = CgiRoute {
    cgiSrc :: ByteString
  , cgiDst :: FilePath
  }

---------------------------------------------------------------

statusNotModified :: Status
statusNotModified = Status 304 "Not Modified"

statusPreconditionFailed :: Status
statusPreconditionFailed = Status 412 "Precondition Failed"

statusRequestedRangeNotSatisfiable :: Status
statusRequestedRangeNotSatisfiable = Status 416 "Requested Range Not Satisfiable"

statusNotImplemented :: Status
statusNotImplemented = Status 501 "Not Implemented"
