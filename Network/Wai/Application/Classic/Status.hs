{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Status (getStatusInfo) where

import Control.Arrow
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 ()
import qualified Data.StaticHash as M
import Network.HTTP.Types
import Network.Wai.Application.Classic.MaybeIter
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils

----------------------------------------------------------------

getStatusInfo :: ClassicAppSpec -> FileAppSpec -> [Lang] -> Status -> IO StatusInfo
getStatusInfo cspec spec langs st = runAny [
    getStatusFile getF dir code langs
  , return (getStatusBS code)
  , return (Just StatusNone)
  ]
  where
    dir = statusFileDir cspec
    getF = getFileInfo spec
    code = statusCode st

----------------------------------------------------------------

statusList :: [Status]
statusList = [
    statusNotAllowed  -- 405 File
  , statusNotFound    -- 404 File
  , statusServerError -- 500 CGI
  , statusBadGateway  -- 502 RevProxy
  ]

----------------------------------------------------------------

statusBSMap :: M.StaticHash Int StatusInfo
statusBSMap = M.fromList $ map (statusCode &&& toRspBody) statusList
  where
    toRspBody s = StatusByteString $ BL.fromChunks [statusMessage s, "\r\n"]

getStatusBS :: Int -> Maybe StatusInfo
getStatusBS code = M.lookup code statusBSMap

----------------------------------------------------------------

statusFileMap :: M.StaticHash Int Path
statusFileMap = M.fromList $ map (statusCode &&& toPath) statusList
  where
    toPath s = fromString $ show (statusCode s) ++ ".html"

getStatusFile :: (Path -> IO (Maybe FileInfo)) -> Path -> Int -> [Lang] -> IO (Maybe StatusInfo)
getStatusFile getF dir code langs = try mfiles
  where
    mfiles = case M.lookup code statusFileMap of
        Nothing   -> []
        Just file -> map ($ (dir </> file)) langs
    try [] = return Nothing
    try (f:fs) = do
        mfi <- getF f
        case mfi of
            Nothing -> try fs
            Just fi -> return . Just $ StatusFile f (fileInfoSize fi)

----------------------------------------------------------------
