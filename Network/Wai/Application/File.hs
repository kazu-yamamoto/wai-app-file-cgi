{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.File (fileApp, FileRoute(..), AppSpec(..)) where

import Control.Monad (mplus)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (length, unpack, pack)
import Data.Enumerator (Iteratee)
import Data.List
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Data.Time
import Network.Wai
import Network.Wai.Application.Date
import Network.Wai.Application.Lang
import Network.Wai.Application.Range
import Network.Wai.Application.Static (defaultMimeTypes, defaultMimeType, MimeType)
import Network.Wai.Application.Types
import Network.Wai.Application.Utils
import System.Directory
import System.FilePath

---------------------------------------------------------------

data FileRoute = FileRoute {
    fileSrc :: ByteString
  , fileDst :: FilePath
  }

type MaybeIter a = Iteratee ByteString IO (Maybe a)
type Rsp = MaybeIter Response

fileApp :: AppSpec -> FileRoute -> Application
fileApp spec filei req = case method of
    "GET"  -> processGET req file ishtml
    "HEAD" -> processHEAD req file ishtml
    _      -> return $ responseLBS statusNotAllowed
                                   textPlain
                                   "Method not allowed"
  where
    file = pathinfoToFile req filei (indexFile spec)
    ishtml = isHTML spec file
    method = requestMethod req

textPlain :: ResponseHeaders
textPlain = [("Content-Type", "text/plain")]

----------------------------------------------------------------

runAny :: [Rsp] -> Iteratee ByteString IO Response
runAny [] = error "runAny"
runAny (a:as) = do
    mrsp <- a
    case mrsp of
      Nothing  -> runAny as
      Just rsp -> return rsp

runAnyMaybe :: [Rsp] -> Rsp
runAnyMaybe []     = return Nothing
runAnyMaybe (a:as) = do
    mx <- a
    case mx of
      Nothing -> runAnyMaybe as
      Just _  -> return mx

----------------------------------------------------------------

processGET :: Request -> FilePath -> Bool -> Iteratee ByteString IO Response
processGET req file ishtml = runAny [
    tryGet req file ishtml langs
  , tryRedirect req file langs
  , notFound
  ]
  where
    langs = map ('.':) (languages req) ++ ["",".en"]

languages :: Request -> [String]
languages req = maybe [] parseLang $ lookupRequestField fkAcceptLanguage req

tryGet :: Request -> FilePath -> Bool -> [String] -> Rsp
tryGet req file True langs = runAnyMaybe $ map (tryGetFile req file) langs
tryGet req file _    _     = tryGetFile req file ""

tryGetFile :: Request -> FilePath -> String -> Rsp
tryGetFile req file lang = do
    let file' = if null lang then file else file ++ lang
    liftIO $ putStrLn $ "GET " ++ file'
    (liftIO $ fileInfo file') |>| \(size, mtime) -> do
      let hdr = getFileHeader file mtime
          mst = ifmodified req size mtime
            ||| ifunmodified req size mtime
            ||| ifrange req size mtime
            ||| unconditional req size mtime
      case mst of
        Just st
          -- FIXME: size modified
          | st == statusOK -> return . Just $ ResponseFile statusOK hdr file'
          -- FIXME skip len
          | st == statusPartialContent -> undefined
          | otherwise -> return . Just $ responseLBS st hdr ""
        _       -> return Nothing -- never reached

getFileHeader :: FilePath -> UTCTime -> ResponseHeaders
getFileHeader file mtime = [
    ("Content-Type", mimeType file)
  , ("Last-Modified", utcToDate mtime)
  ]

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
  Just (skip,len) -> Just statusPartialContent -- FIXME skip len

----------------------------------------------------------------

tryRedirect :: Request -> FilePath -> [String] -> Rsp
tryRedirect req file langs = do
    exist <- liftIO $ doesDirectoryExist file
    if exist
       then return . Just $ responseLBS statusMovedPermanently
            [("Content-Type", "text/plain")
            ,("Location", BS.pack $ file ++ "/")]
            "Moved permanently"
       else return Nothing

----------------------------------------------------------------

processHEAD :: Request -> FilePath -> Bool -> Iteratee ByteString IO Response
processHEAD req file ishtml = runAny [
    tryHead req file ishtml langs
  , tryRedirect req file langs
  , notFound ] -- always Just
  where
    langs = map ('.':) (languages req) ++ ["",".en"]

tryHead :: Request -> FilePath -> Bool -> [String] -> Rsp
tryHead req file True langs = runAnyMaybe $ map (tryHeadFile req file) langs
tryHead req file _    _     = tryHeadFile req file ""

tryHeadFile :: Request -> FilePath -> String -> Rsp
tryHeadFile req file lang = do
    let file' = if null lang then file else file ++ lang
    liftIO $ putStrLn $ "HEAD " ++ file'
    (liftIO $ fileInfo file') |>| \(size, mtime) -> do
      let hdr = getFileHeader file mtime
          mst = ifmodified req size mtime
            ||| Just statusOK
      case mst of
        Just st -> return . Just $ responseLBS st hdr ""
        _       -> return Nothing -- never reached

----------------------------------------------------------------

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

----------------------------------------------------------------

(|>|) :: MaybeIter a -> (a -> MaybeIter b) -> MaybeIter b
a |>| act = do
    v <- a
    case v of
      Nothing -> return Nothing
      Just x  -> act x

(|||) :: Maybe Status -> Maybe Status -> Maybe Status
(|||) = mplus

----------------------------------------------------------------

ifModifiedSince :: Request -> Maybe UTCTime
ifModifiedSince = lookupAndParseDate fkIfModifiedSince

ifUnmodifiedSince :: Request -> Maybe UTCTime
ifUnmodifiedSince = lookupAndParseDate fkIfUnmodifiedSince

ifRange :: Request -> Maybe UTCTime
ifRange = lookupAndParseDate fkIfRange

lookupAndParseDate :: ByteString -> Request -> Maybe UTCTime
lookupAndParseDate key req = lookupRequestField key req >>= parseDate
