{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Network.Wai

data AppSpec = AppSpec {
    softwareName :: ByteString
  , indexFile :: FilePath
  , isHTML :: FilePath -> Bool
  , logger :: Request -> RspSpec -> IO ()
  }

data FileRoute = FileRoute {
    fileSrc :: ByteString
  , fileDst :: FilePath
  }

data CgiRoute = CgiRoute {
    cgiSrc :: ByteString
  , cgiDst :: FilePath
  }

data RspSpec = RspSpec {
    rspStatus :: Status
  , rspHeaders :: ResponseHeaders
  , rspBody :: RspBody
  }

data RspBody = NoBody | BodyLBS BL.ByteString | BodyFile FilePath Integer

---------------------------------------------------------------

statusNotModified :: Status
statusNotModified = Status 304 "Not Modified"

statusPreconditionFailed :: Status
statusPreconditionFailed = Status 412 "Precondition Failed"

statusRequestedRangeNotSatisfiable :: Status
statusRequestedRangeNotSatisfiable = Status 416 "Requested Range Not Satisfiable"

statusNotImplemented :: Status
statusNotImplemented = Status 501 "Not Implemented"
