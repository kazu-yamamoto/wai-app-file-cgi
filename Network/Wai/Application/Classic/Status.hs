{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Wai.Application.Classic.Status (getStatusInfo) where

import Control.Applicative
import Control.Arrow
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 ()
import Data.Maybe
import qualified Data.StaticHash as M
import Network.HTTP.Types
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import Prelude hiding (catch)

instance Alternative IO where
  empty = goNext
  x <|> y = x `catch` (\(_ :: SomeException) -> y)

goNext :: IO a
goNext = undefined

----------------------------------------------------------------

getStatusInfo :: ClassicAppSpec -> FileAppSpec -> [Lang] -> Status -> IO StatusInfo
getStatusInfo cspec spec langs st = getStatusFile getF dir code langs
                                <|> (return $ getStatusBS code)
                                <|> return StatusNone
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

getStatusBS :: Int -> StatusInfo
getStatusBS code = case M.lookup code statusBSMap of
    Nothing -> error "getStatusBS"
    Just x  -> x

----------------------------------------------------------------

statusFileMap :: M.StaticHash Int Path
statusFileMap = M.fromList $ map (statusCode &&& toPath) statusList
  where
    toPath s = fromString $ show (statusCode s) ++ ".html"

getStatusFile :: (Path -> IO FileInfo) -> Path -> Int -> [Lang] -> IO StatusInfo
getStatusFile getF dir code langs = tryFile mfiles
  where
    mfiles = case M.lookup code statusFileMap of
        Nothing   -> []
        Just file -> map ($ (dir </> file)) langs
    tryFile = foldr func goNext
    func f io = StatusFile f . fileInfoSize <$> getF f <|> io

----------------------------------------------------------------
