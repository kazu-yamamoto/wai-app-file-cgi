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
    "GET"  -> processGET req file ishtml
    "HEAD" -> processHEAD req file ishtml
    _      -> return $ responseLBS statusNotAllowed textPlain "Method not allowed"
  where
    file = pathinfoToFile req filei (indexFile spec)
    -- FIXME redirect information should be generated here
    ishtml = isHTML spec file
    method = requestMethod req

----------------------------------------------------------------

processGET :: Request -> FilePath -> Bool -> Iteratee ByteString IO Response
processGET req file ishtml = runAny [
    tryGet req file ishtml langs
  , tryRedirect req file langs
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
      let hdr = newHeader file mtime
          mst = ifmodified req size mtime
            ||| Just statusOK
      case mst of
        Just st -> just $ responseLBS st hdr ""
        _       -> nothing -- never reached

----------------------------------------------------------------

tryRedirect  :: Request -> FilePath -> [String] -> Rsp
tryRedirect req file langs =
    redirectURI file >>| \rfile -> runAnyMaybe $ map (tryRedirectFile req rfile file) langs

tryRedirectFile :: Request -> FilePath -> FilePath -> String -> Rsp
tryRedirectFile req rpath file lang = do
    let file' = file ++ lang
    minfo <- liftIO $ fileInfo file'
    case minfo of
      Nothing -> nothing
      Just _  -> just $ responseLBS statusMovedPermanently hdr ""
  where
    hdr = [("Location", toURL rpath)]
    (+++) = BS.append
    toURL path = "http://"
             +++ serverName req
             +++ ":"
             +++ (BS.pack . show . serverPort) req
             +++ BS.pack path

----------------------------------------------------------------

notFound :: Rsp
notFound = just $ responseLBS statusNotFound textPlain "Not found"
