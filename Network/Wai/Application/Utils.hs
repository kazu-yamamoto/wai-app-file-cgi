module Network.Wai.Application.Utils where

import Data.ByteString (ByteString)
import Data.Enumerator (($$))
import Data.Enumerator (Iteratee,Enumeratee,joinI)
import Data.Time
import Data.Time.Clock.POSIX
import Network.Wai
import System.Directory
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

lookupRequestField :: ByteString -> Request -> Maybe ByteString
lookupRequestField x req = lookupField x hdrs
  where
    hdrs = requestHeaders req

lookupField :: ByteString -> RequestHeaders -> Maybe ByteString
lookupField x (((CIByteString _ l), val):kvs)
  | x == l       = Just val
  | otherwise    = lookupField x kvs
lookupField _ [] = Nothing

----------------------------------------------------------------

infixr 0 =$

(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
ee =$ ie = joinI $ ee $$ ie

----------------------------------------------------------------

infixr 6 $.

($.) :: (a -> b) -> a -> b
($.) = ($)

