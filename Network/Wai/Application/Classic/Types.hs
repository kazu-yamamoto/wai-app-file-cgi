module Network.Wai.Application.Classic.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Network.HTTP.Date
import Network.HTTP.Types
import Network.Wai.Logger.Prefork

data AppSpec = AppSpec {
    -- | Name specified to Server: in HTTP response.
    softwareName :: ByteString
    -- | A file name of an index file.
  , indexFile :: ByteString
    -- | Whether this is an HTML or not.
  , isHTML :: ByteString -> Bool
    -- | A function for logging. The third argument is a body size.
  , logger :: ApacheLogger
    -- | A function to obtain information about a file.
  , getFileInfo :: ByteString -> IO (Maybe FileInfo)
  }

data FileInfo = FileInfo {
    fileInfoName :: FilePath
  , fileInfoSize :: Integer
  , fileInfoTime :: HTTPDate
  }

data FileRoute = FileRoute {
    -- | Path prefix to be matched to 'pathInfo'.
    fileSrc :: ByteString
    -- | Path prefix to an actual file system.
  , fileDst :: ByteString
  }

data CgiRoute = CgiRoute {
    -- | Path prefix to be matched to 'pathInfo'.
    cgiSrc :: ByteString
    -- | Path prefix to an actual file system.
  , cgiDst :: ByteString
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
  | BodyFile String Range

data Range =
    -- | Entire file showing its file size
    Entire Integer
    -- | A part of a file taking offset and length
  | Part Integer Integer
