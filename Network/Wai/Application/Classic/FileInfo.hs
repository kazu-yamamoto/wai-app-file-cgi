module Network.Wai.Application.Classic.FileInfo where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (unpack)
import Network.HTTP.Date
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Range
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils

----------------------------------------------------------------

data StatusAux = Full Status | Partial Integer Integer deriving Show

ifmodified :: Request -> Integer -> HTTPDate -> Maybe StatusAux
ifmodified req size mtime = do
    date <- ifModifiedSince req
    if date /= mtime
       then unconditional req size mtime
       else Just (Full statusNotModified)

ifunmodified :: Request -> Integer -> HTTPDate -> Maybe StatusAux
ifunmodified req size mtime = do
    date <- ifUnmodifiedSince req
    if date == mtime
       then unconditional req size mtime
       else Just (Full statusPreconditionFailed)

ifrange :: Request -> Integer -> HTTPDate -> Maybe StatusAux
ifrange req size mtime = do
    date <- ifRange req
    rng  <- lookupRequestField fkRange req
    if date == mtime
       then Just (Full statusOK)
       else range size rng

unconditional :: Request -> Integer -> HTTPDate -> Maybe StatusAux
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

addIndex :: FileAppSpec -> ByteString -> ByteString
addIndex spec path
  | hasTrailingPathSeparator path = path </> indexFile spec
  | otherwise                     = path

redirectPath :: FileAppSpec -> ByteString -> Maybe ByteString
redirectPath spec path
  | hasTrailingPathSeparator path = Nothing
  | otherwise                     = Just (path </> indexFile spec)
