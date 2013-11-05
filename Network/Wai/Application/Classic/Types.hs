{-# LANGUAGE CPP #-}

module Network.Wai.Application.Classic.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Network.HTTP.Date
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Path
#ifdef REV_PROXY
import qualified Network.HTTP.Conduit as H
#endif

----------------------------------------------------------------

data ClassicAppSpec = ClassicAppSpec {
    -- | Name specified to Server: in HTTP response.
    softwareName :: ByteString
    -- | A function for logging. The third argument is a body size.
  , logger :: Request -> Status -> Maybe Integer -> IO ()
    -- | A function to get HTTP's GMT Date.
  , statusFileDir :: Path
  }

data StatusInfo =
  -- | HTTP status body is created from 'LB.ByteString'.
    StatusByteString BL.ByteString
  -- | HTTP status body is created from 'FilePath'.
  | StatusFile Path Integer
  -- | No HTTP status body.
  | StatusNone
  deriving (Eq,Show)

----------------------------------------------------------------

data FileAppSpec = FileAppSpec {
    -- | A file name of an index file.
    indexFile :: Path
    -- | Whether this is an HTML or not.
  , isHTML :: Path -> Bool
    -- | A function to obtain information about a file.
    --   If information is not obtained, an IO exception should be raised.
  , getFileInfo :: Path -> IO FileInfo
  }

data FileInfo = FileInfo {
    fileInfoName :: !Path
  , fileInfoSize :: !Integer
  , fileInfoTime :: !HTTPDate
  , fileInfoDate :: !ByteString
  } deriving (Eq, Show)

data FileRoute = FileRoute {
    -- | Path prefix to be matched to 'rawPathInfo'.
    fileSrc :: Path
    -- | Path prefix to an actual file system.
  , fileDst :: Path
  } deriving (Eq,Show)

----------------------------------------------------------------

data RedirectRoute = RedirectRoute {
    -- | Path prefix to be matched to 'rawPathInfo'.
    redirectSrc :: Path
    -- | Path prefix to an actual file system.
  , redirectDst :: Path
  } deriving (Eq,Show)

----------------------------------------------------------------

data CgiAppSpec = CgiAppSpec {
    -- | A file name of the default CGI.
    indexCgi :: Path
  } deriving (Eq,Show)

data CgiRoute = CgiRoute {
    -- | Path prefix to be matched to 'rawPathInfo'.
    cgiSrc :: Path
    -- | Path prefix to an actual file system.
  , cgiDst :: Path
  } deriving (Eq,Show)

----------------------------------------------------------------

#ifdef REV_PROXY
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
  } deriving (Eq,Show)
#endif

----------------------------------------------------------------

data RspSpec = RspSpec {
    -- | Response status.
    rspStatus :: Status
    -- | Response body.
  , rspBody :: RspBody
  } deriving (Eq,Show)

data RspBody =
    NoBody
  | BodyStatus
  | BodyFileNoBody ResponseHeaders
  | BodyFile ResponseHeaders Path Range
  deriving (Eq,Show)

data Range =
    -- | Entire file showing its file size
    Entire Integer
    -- | A part of a file taking offset and length
  | Part Integer -- offset
         Integer -- length
         Integer -- total
  deriving (Eq,Show)

----------------------------------------------------------------

type Lang = Path -> Path
