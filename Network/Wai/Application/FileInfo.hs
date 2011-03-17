module Network.Wai.Application.FileInfo where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Data.Time.Clock.POSIX
import Network.Wai
import Network.Wai.Application.Header
import Network.Wai.Application.Range
import Network.Wai.Application.Types
import System.Directory
import System.FilePath
import System.Posix.Files

----------------------------------------------------------------

fileInfo :: FilePath -> IO (Maybe (Integer, UTCTime))
fileInfo file = do
    exist <- doesFileExist file
    if exist
       then do
         fs <- getFileStatus file
         let size = fromIntegral . fileSize $ fs
             mtime = posixSecondsToUTCTime . realToFrac . modificationTime $ fs
         return $ Just (size, mtime)
       else return Nothing

----------------------------------------------------------------

ifmodified :: Request -> Integer -> UTCTime -> Maybe Status
ifmodified req size mtime = do
    date <- ifModifiedSince req
    if date /= mtime
       then unconditional req size mtime
       else Just statusNotModified

ifunmodified :: Request -> Integer -> UTCTime -> Maybe Status
ifunmodified req size mtime = do
    date <- ifUnmodifiedSince req
    if date == mtime
       then unconditional req size mtime
       else Just statusPreconditionFailed

ifrange :: Request -> Integer -> UTCTime -> Maybe Status
ifrange req size mtime = do
    date <- ifRange req
    rng  <- lookupRequestField fkRange req
    if date == mtime
       then Just statusOK
       else range size rng

unconditional :: Request -> Integer -> UTCTime -> Maybe Status
unconditional req size _ =
    maybe (Just statusOK) (range size) $ lookupRequestField fkRange req

range :: Integer -> ByteString -> Maybe Status
range size rng = case skipAndSize rng size of
  Nothing         -> Just statusRequestedRangeNotSatisfiable
  Just (_,_)      -> Just statusNotImplemented
--  Just (skip,len) -> Just statusPartialContent -- FIXME skip len


----------------------------------------------------------------

pathinfoToFile :: Request -> FileRoute -> String -> FilePath
pathinfoToFile req filei index = file
  where
    path = pathInfo req
    src = fileSrc filei
    dst = fileDst filei
    path' = dst </> (drop (BS.length src) $ BS.unpack path)
    file = if hasTrailingPathSeparator path'
           then path' </> index
           else path'

redirectURI :: FilePath -> Maybe FilePath
redirectURI path =
   if hasTrailingPathSeparator path
      then Nothing
      else Just $ path ++ "/"
