{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.File (fileApp, FileInfo(..)) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (length, unpack)
import Network.Wai
import Network.Wai.Application.Static (defaultMimeTypeByExt)
import System.FilePath

data FileInfo = FileInfo {
    fileSrc :: ByteString
  , fileDst :: FilePath
  , indexFile :: FilePath
  }

fileApp :: FileInfo -> Application
fileApp filei req = return $ ResponseFile statusOK
                                          [("Content-Type", typ)]
                                          file
  where
    file = pathinfoToFile req filei
    typ  = defaultMimeTypeByExt file

pathinfoToFile :: Request -> FileInfo -> FilePath
pathinfoToFile req filei= file
  where
    path = pathInfo req
    src = fileSrc filei
    dst = fileDst filei
    index = indexFile filei
    path' = dst </> (drop (BS.length src) $ BS.unpack path)
    file = if hasTrailingPathSeparator path'
           then path' </> index
           else path'

