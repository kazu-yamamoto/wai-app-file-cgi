{-# LANGUAGE CPP #-}

module Network.Wai.Application.Classic.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Network.HTTP.Client as H
import Network.HTTP.Date
import Network.Wai.Handler.Warp (FileInfo(..))
import Network.Wai.Application.Classic.Path

----------------------------------------------------------------

data ClassicAppSpec = ClassicAppSpec {
    -- | Name specified to Server: in HTTP response.
    softwareName :: ByteString
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
  }

data Fileinfo = Fileinfo {
    fileinfoName :: !Path
  , fileinfoSize :: !Integer
  , fileinfoTime :: !HTTPDate
  , fileinfoDate :: !ByteString
  } deriving (Eq, Show)

fromFileInfo :: FileInfo -> Fileinfo
fromFileInfo x = Fileinfo {
    fileinfoName = fromString $ fileInfoName x
  , fileinfoSize = fileInfoSize x
  , fileinfoTime = fileInfoTime x
  , fileinfoDate = fileInfoDate x
  }

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

----------------------------------------------------------------

type Lang = Path -> Path
