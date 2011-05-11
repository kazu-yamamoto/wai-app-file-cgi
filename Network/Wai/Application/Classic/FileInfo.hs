module Network.Wai.Application.Classic.FileInfo where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (unpack)
import qualified Data.ByteString.Char8 as BS (unpack)
import Data.Word
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Range
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import System.Posix.Files
import System.Posix.Types

----------------------------------------------------------------

-- This function is slow.
-- So, let's avoid using doesFileExist which uses getFilesStatus.
fileInfo :: ByteString -> IO (Maybe (Integer, UnixTime))
fileInfo file = handle nothing $ do
    fs <- getFileStatus (BS.unpack file)
    if doesExist fs
       then return $ Just (size fs, mtime fs)
       else return Nothing
  where
    nothing :: IOException -> IO (Maybe (Integer, UnixTime))
    nothing _ = return Nothing
    size = fromIntegral . fileSize
    mtime = epochTimeToUnixTime . modificationTime
    doesExist = not . isDirectory

epochTimeToUnixTime :: EpochTime -> UnixTime
epochTimeToUnixTime x = UnixTime d t
  where
    w64 :: Word64
    w64 = truncate . toRational $ x
    (d',t') = w64 `divMod` 86400
    d = fromIntegral d'
    t = fromIntegral t'

----------------------------------------------------------------

data StatusAux = Full Status | Partial Integer Integer deriving Show

ifmodified :: Request -> Integer -> UnixTime -> Maybe StatusAux
ifmodified req size mtime = do
    date <- ifModifiedSince req
    if date /= mtime
       then unconditional req size mtime
       else Just (Full statusNotModified)

ifunmodified :: Request -> Integer -> UnixTime -> Maybe StatusAux
ifunmodified req size mtime = do
    date <- ifUnmodifiedSince req
    if date == mtime
       then unconditional req size mtime
       else Just (Full statusPreconditionFailed)

ifrange :: Request -> Integer -> UnixTime -> Maybe StatusAux
ifrange req size mtime = do
    date <- ifRange req
    rng  <- lookupRequestField fkRange req
    if date == mtime
       then Just (Full statusOK)
       else range size rng

unconditional :: Request -> Integer -> UnixTime -> Maybe StatusAux
unconditional req size _ =
    maybe (Just (Full statusOK)) (range size) $ lookupRequestField fkRange req

range :: Integer -> ByteString -> Maybe StatusAux
range size rng = case skipAndSize rng size of
  Nothing         -> Just (Full statusRequestedRangeNotSatisfiable)
  Just (skip,len) -> Just (Partial skip len)

----------------------------------------------------------------

pathinfoToFilePath :: Request -> FileRoute -> ByteString
pathinfoToFilePath req filei = path'
  where
    path = rawPathInfo req
    src = fileSrc filei
    dst = fileDst filei
    path' = dst </> BS.drop (BS.length src) path

addIndex :: AppSpec -> ByteString -> ByteString
addIndex spec path
  | hasTrailingPathSeparator path = path </> indexFile spec
  | otherwise                     = path

redirectPath :: AppSpec -> ByteString -> Maybe ByteString
redirectPath spec path
  | hasTrailingPathSeparator path = Nothing
  | otherwise                     = Just (path </> indexFile spec)
