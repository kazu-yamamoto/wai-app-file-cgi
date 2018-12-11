{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Application.Classic.Status (getStatusInfo) where

import Control.Applicative
import Control.Arrow
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 ()
import qualified Data.StaticHash as M
import Network.HTTP.Types
import Network.Wai (Request)
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types
import Network.Wai.Handler.Warp

----------------------------------------------------------------

getStatusInfo :: ClassicAppSpec -> Request -> [Lang] -> Status -> IO StatusInfo
getStatusInfo cspec req langs st = getStatusFile getF dir code langs
                               <|> getStatusBS code
                               <|> return StatusNone
  where
    dir = statusFileDir cspec
    getF = getFileInfo req
    code = statusCode st

----------------------------------------------------------------

statusList :: [Status]
statusList = [
    methodNotAllowed405    -- File
  , notFound404            -- File
  , internalServerError500 -- CGI
  , badGateway502          -- RevProxy
  ]

----------------------------------------------------------------

statusBSMap :: M.StaticHash Int StatusInfo
statusBSMap = M.fromList $ map (statusCode &&& toRspBody) statusList
  where
    toRspBody s = StatusByteString $ BL.fromChunks [statusMessage s, "\r\n"]

getStatusBS :: Int -> IO StatusInfo
getStatusBS code = case M.lookup code statusBSMap of
    Nothing -> throwIO $ userError "getStatusBS"
    Just x  -> return x

----------------------------------------------------------------

statusFileMap :: M.StaticHash Int Path
statusFileMap = M.fromList $ map (statusCode &&& toPath) statusList
  where
    toPath s = fromString $ show (statusCode s) ++ ".html"

getStatusFile :: (FilePath -> IO FileInfo) -> Path -> Int -> [Lang] -> IO StatusInfo
getStatusFile getF dir code langs = tryFile mfiles
  where
    mfiles = case M.lookup code statusFileMap of
        Nothing   -> []
        Just file -> map ($ (dir </> file)) langs
    tryFile = foldr func empty
    func f io = StatusFile f . fileInfoSize <$> getF f' <|> io
      where
        f' = pathString f

----------------------------------------------------------------
