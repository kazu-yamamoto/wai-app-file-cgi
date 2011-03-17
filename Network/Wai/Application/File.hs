{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.File (fileApp, FileRoute(..), AppSpec(..)) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Enumerator (Iteratee)
import Network.Wai
import Network.Wai.Application.FileInfo
import Network.Wai.Application.Header
import Network.Wai.Application.MaybeIter
import Network.Wai.Application.Types

----------------------------------------------------------------

fileApp :: AppSpec -> FileRoute -> Application
fileApp spec filei req = case method of
    "GET"  -> processGET  req file ishtml rfile
    "HEAD" -> processHEAD req file ishtml rfile
    _      -> return $ responseLBS statusNotAllowed textPlain "Method not allowed"
  where
    method = requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path

----------------------------------------------------------------

processGET :: Request -> FilePath -> Bool -> (Maybe FilePath) -> Iteratee ByteString IO Response
processGET req file ishtml rfile = runAny [
    tryGet req file ishtml langs
  , tryRedirect req rfile langs
  , notFound
  ]
  where
    langs = map ('.':) (languages req) ++ ["",".en"]

tryGet :: Request -> FilePath -> Bool -> [String] -> Rsp
tryGet req file True langs = runAnyMaybe $ map (tryGetFile req file) langs
tryGet req file _    _     = tryGetFile req file ""

tryGetFile :: Request -> FilePath -> String -> Rsp
tryGetFile req file lang = do
    let file' = if null lang then file else file ++ lang
    liftIO $ putStrLn $ "GET " ++ file' -- xxx
    (liftIO $ fileInfo file') |>| \(size, mtime) -> do
      let hdr = newHeader file mtime
          mst = ifmodified req size mtime
            ||| ifunmodified req size mtime
            ||| ifrange req size mtime
            ||| unconditional req size mtime
      case mst of
        Just st
          | st == statusOK -> just $ ResponseFile statusOK hdr file'
          -- FIXME skip len
          | st == statusPartialContent -> undefined
          | otherwise      -> just $ responseLBS st hdr ""
        _                  -> nothing -- never reached

----------------------------------------------------------------

processHEAD :: Request -> FilePath -> Bool -> (Maybe FilePath) -> Iteratee ByteString IO Response
processHEAD req file ishtml rfile = runAny [
    tryHead req file ishtml langs
  , tryRedirect req rfile langs
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
      let hdr = newHeader file mtime
          mst = ifmodified req size mtime
            ||| Just statusOK
      case mst of
        Just st -> just $ responseLBS st hdr ""
        _       -> nothing -- never reached

----------------------------------------------------------------

tryRedirect  :: Request -> (Maybe FilePath) -> [String] -> Rsp
tryRedirect _   Nothing     _     = nothing
tryRedirect req (Just file) langs =
    runAnyMaybe $ map (tryRedirectFile req file) langs

tryRedirectFile :: Request -> FilePath -> String -> Rsp
tryRedirectFile req file lang = do
    let file' = file ++ lang
    minfo <- liftIO $ fileInfo file'
    case minfo of
      Nothing -> nothing
      Just _  -> just $ responseLBS statusMovedPermanently hdr ""
  where
    hdr = [("Location", redirectURL)]
    (+++) = BS.append
    redirectURL = "http://"
              +++ serverName req
              +++ ":"
              +++ (BS.pack . show . serverPort) req
              +++ pathInfo req
              +++ "/"

----------------------------------------------------------------

notFound :: Rsp
notFound = just $ responseLBS statusNotFound textPlain "Not found"
