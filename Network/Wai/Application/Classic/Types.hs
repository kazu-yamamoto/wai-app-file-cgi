module Network.Wai.Application.Classic.Types where

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
