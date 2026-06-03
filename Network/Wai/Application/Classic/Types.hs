{-# LANGUAGE CPP #-}

module Network.Wai.Application.Classic.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Network.HTTP.Client as H
import Network.Wai.Application.Classic.Path

----------------------------------------------------------------

data ClassicAppSpec = ClassicAppSpec
    { softwareName :: ByteString
    -- ^ Name specified to Server: in HTTP response.
    , statusFileDir :: Path
    -- ^ A function to get HTTP's GMT Date.
    }

data StatusInfo
    = -- | HTTP status body is created from 'LB.ByteString'.
      StatusByteString BL.ByteString
    | -- | HTTP status body is created from 'FilePath'.
      StatusFile Path Integer
    | -- | No HTTP status body.
      StatusNone
    deriving (Eq, Show)

----------------------------------------------------------------

data FileAppSpec = FileAppSpec
    { indexFile :: Path
    -- ^ A file name of an index file.
    , isHTML :: Path -> Bool
    -- ^ Whether this is an HTML or not.
    }

-- | 'FileRoute' describes the relationship between Paths requested via HTTP
--   and paths to serve on disk. Specifically, it maps a /single/ path
--   prefix to a destination path, so the 'FileRoute' needs to be applicable
--   to the current request,
--   e.g. @FileRoute "\/static\/" "\/var\/www\/static\/"@ will yield a 404 if
--   @"/page.html"@ is requested.
--   In the simplest case @FileRoute "\/" "\/var\/www\/"@ routes everything
--   to a single destination path.
--
--   When resolving routes, no filesystem reads are performed, so path
--   type needs to be inferred from the path.
--
--   * If 'fileSrc' and 'fileDst' have a trailing slash, source and
--     destination are assumed to be directories (this e.g. enables
--     fallback to index files).
--
--   * If 'fileSrc' and 'fileDst' lack a trailing slash, they are
--     assumed to be files. This is supported since 3.2.0.
--
--   If the path type inferrable from 'FileRoute' does not match
--   the type of the destination path on disk, routes will not
--   resolve correctly. If the inferrable type doesn't match
--   between 'fileSrc' and 'fileDst', routing may also misbehave.
data FileRoute = FileRoute
    { fileSrc :: Path
    -- ^ Path (prefix) to be matched to 'rawPathInfo'.
    , fileDst :: Path
    -- ^ Path to the target directory or file.
    }
    deriving (Eq, Show)

----------------------------------------------------------------

data RedirectRoute = RedirectRoute
    { redirectSrc :: Path
    -- ^ Path prefix to be matched to 'rawPathInfo'.
    , redirectDst :: Path
    -- ^ Path prefix to an actual file system.
    }
    deriving (Eq, Show)

----------------------------------------------------------------

data CgiAppSpec = CgiAppSpec
    { indexCgi :: Path
    -- ^ A file name of the default CGI.
    }
    deriving (Eq, Show)

data CgiRoute = CgiRoute
    { cgiSrc :: Path
    -- ^ Path prefix to be matched to 'rawPathInfo'.
    , cgiDst :: Path
    -- ^ Path prefix to an actual file system.
    }
    deriving (Eq, Show)

----------------------------------------------------------------

data RevProxyAppSpec = RevProxyAppSpec
    { revProxyManager :: H.Manager
    -- ^ Connection manager
    }

data RevProxyRoute = RevProxyRoute
    { revProxySrc :: Path
    -- ^ Path prefix to be matched to 'rawPathInfo'.
    , revProxyDst :: Path
    -- ^ Destination path prefix.
    , revProxyDomain :: ByteString
    -- ^ Destination domain name.
    , revProxyPort :: Int
    -- ^ Destination port number.
    }
    deriving (Eq, Show)

----------------------------------------------------------------

type Lang = Path -> Path
