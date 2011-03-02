{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.File (fileApp, FileRoute(..), AppSpec(..)) where

import Control.Monad (mplus)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (length, unpack, pack)
import Data.Enumerator (Iteratee)
import Data.List (foldl1')
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Network.Wai
import Network.Wai.Application.Static (defaultMimeTypes, defaultMimeType, MimeType)
import System.Directory
import System.FilePath

import Network.Wai.Application.Types

data FileRoute = FileRoute {
    fileSrc :: ByteString
  , fileDst :: FilePath
  }

type Rsp = Iteratee ByteString IO (Maybe Response)

fileApp :: AppSpec -> FileRoute -> Application
fileApp spec filei req = case method of
    "GET"  -> processGET req file
    "HEAD" -> return $ responseLBS statusNotAllowed
                                   [("Content-Type", "text/plain")]
                                   "Method not allowed"
    _      -> return $ responseLBS statusNotAllowed
                                   [("Content-Type", "text/plain")]
                                   "Method not allowed"
  where
    file = pathinfoToFile req filei (indexFile spec)
    method = requestMethod req

runAny :: [Rsp] -> Iteratee ByteString IO Response
runAny [] = error "runAny"
runAny (a:as) = do
    mrsp <- a
    case mrsp of
      Nothing  -> runAny as
      Just rsp -> return rsp

processGET :: Request -> FilePath -> Iteratee ByteString IO Response
processGET req file = runAny [
    tryGet req file
  , tryRedirect req file
  , notFound
  ]

textPlain :: ResponseHeaders
textPlain = [("Content-Type", "text/plain")]

tryGet :: Request -> FilePath -> Rsp
tryGet req file = do
    exist <- liftIO $ doesFileExist file
    if exist
       then return . Just $ ResponseFile statusOK [("Content-Type", typ)] file
       else return Nothing
  where
    typ = mimeType file

tryRedirect :: Request -> FilePath -> Rsp
tryRedirect req file = do
    exist <- liftIO $ doesDirectoryExist file
    if exist
       then return . Just $ responseLBS statusMovedPermanently
            [("Content-Type", "text/plain")
            ,("Location", BS.pack $ file ++ "/")]
            "Moved permanently"
       else return Nothing

notFound :: Rsp
notFound = return . Just $ responseLBS statusNotFound textPlain "Not found"

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

mimeType :: FilePath -> MimeType
mimeType file =fromMaybe defaultMimeType . foldl1' mplus . map lok $ targets
  where
    targets = extensions file
    lok x = M.lookup x defaultMimeTypes

extensions :: FilePath -> [String]
extensions file = entire : exts
  where
    exts' = split ('.'==) file
    exts = if exts' == [] then [] else tail exts'
    entire = foldr (\x y -> x ++ '.' : y) "" exts

split :: (Char -> Bool) -> String -> [String]
split _ "" = []
split p xs = case break p xs of
    (ys,"")   -> [ys]
    (ys,_:zs) -> ys : split p zs
