module Network.Wai.Application.Classic.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Network.HTTP.Date
import qualified Network.HTTP.Enumerator as H
import Network.HTTP.Types
import Network.Wai.Application.Classic.Utils
import Network.Wai.Logger.Prefork

----------------------------------------------------------------

data ClassicAppSpec = ClassicAppSpec {
    -- | Name specified to Server: in HTTP response.
    softwareName :: ByteString
    -- | A function for logging. The third argument is a body size.
  , logger :: ApacheLogger
    -- | A function to get the HTTP body of status.
  , statusFileDir :: Path
  }

data StatusInfo =
  -- | HTTP status body is created from 'LB.ByteString'.
    StatusByteString BL.ByteString
  -- | HTTP status body is created from 'FilePath'.
  | StatusFile Path Integer
  -- | No HTTP status body.
  | StatusNone

----------------------------------------------------------------

data FileAppSpec = FileAppSpec {
    -- | A file name of an index file.
    indexFile :: Path
    -- | Whether this is an HTML or not.
  , isHTML :: Path -> Bool
    -- | A function to obtain information about a file
  , getFileInfo :: Path -> IO (Maybe FileInfo)
  }

data FileInfo = FileInfo {
    fileInfoName :: Path
  , fileInfoSize :: Integer
  , fileInfoTime :: HTTPDate
  }

data FileRoute = FileRoute {
    -- | Path prefix to be matched to 'rawPathInfo'.
    fileSrc :: Path
    -- | Path prefix to an actual file system.
  , fileDst :: Path
  }

----------------------------------------------------------------

data CgiRoute = CgiRoute {
    -- | Path prefix to be matched to 'rawPathInfo'.
    cgiSrc :: Path
    -- | Path prefix to an actual file system.
  , cgiDst :: Path
  }

----------------------------------------------------------------

data RevProxyAppSpec = RevProxyAppSpec {
    -- | Connection manager
    revProxyManager :: H.Manager
  }

data RevProxyRoute = RevProxyRoute {
    -- | Path prefix to be matched to 'rawPathInfo'.
    revProxySrc :: Path
    -- | Destination path prefix.
  , revProxyDst :: Path
    -- | Destination domain name.
  , revProxyDomain :: ByteString
    -- | Destination port number.
  , revProxyPort :: Int
  }

----------------------------------------------------------------

data RspSpec = RspSpec {
    -- | Response status.
    rspStatus :: Status
    -- | Response body.
  , rspBody :: RspBody
  }

data RspBody =
    NoBody
  | BodyStatus
  | BodyFileNoBody ResponseHeaders
  | BodyFile ResponseHeaders Path Range

data Range =
    -- | Entire file showing its file size
    Entire Integer
    -- | A part of a file taking offset and length
  | Part Integer Integer

----------------------------------------------------------------

type Lang = Path -> Path
