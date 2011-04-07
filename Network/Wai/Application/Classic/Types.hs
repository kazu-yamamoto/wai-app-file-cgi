module Network.Wai.Application.Classic.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Network.HTTP.Types
import Network.Wai

data AppSpec = AppSpec {
    -- | Name specified to Server: in HTTP response.
    softwareName :: ByteString
    -- | A file name of an index file.
  , indexFile :: FilePath
    -- | Whether this is an HTML or not.
  , isHTML :: FilePath -> Bool
    -- | A function for logging.
  , logger :: Request -> Status -> IO ()
  }

data FileRoute = FileRoute {
    -- | Path prefix to be matched to 'pathInfo'.
    fileSrc :: ByteString
    -- | Path prefix to an actual file system.
  , fileDst :: FilePath
  }

data CgiRoute = CgiRoute {
    -- | Path prefix to be matched to 'pathInfo'.
    cgiSrc :: ByteString
    -- | Path prefix to an actual file system.
  , cgiDst :: FilePath
  }

data RspSpec = RspSpec {
    -- | Response status.
    rspStatus :: Status
    -- | Response headers.
  , rspHeaders :: ResponseHeaders
    -- | Response body.
  , rspBody :: RspBody
  }

data RspBody =
    -- | Body does not exist.
    NoBody
    -- | Body as Lazy ByteString.
  | BodyLBS BL.ByteString
    -- | Body as a file.
  | BodyFile FilePath Integer
