{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.File (fileApp, FileRoute(..), AppSpec(..)) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL ()
import Network.Wai
import Network.Wai.Application.FileInfo
import Network.Wai.Application.Header
import Network.Wai.Application.MaybeIter
import Network.Wai.Application.Types

----------------------------------------------------------------

fileApp :: AppSpec -> FileRoute -> Application
fileApp spec filei req = do
    Ctl st hdr body <- case method of
        "GET"  -> processGET  req file ishtml rfile
        "HEAD" -> processHEAD req file ishtml rfile
        _      -> return $ notAllowed
    let hdr' = addHeader hdr
    case body of
        CtlNone           -> return $ responseLBS st hdr' ""
        CtlBody bd        -> return $ responseLBS st hdr' bd
        CtlFile afile _   -> return $ ResponseFile st hdr' afile -- FIXME size
  where
    method = requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    addHeader hdr = ("Server", softwareName spec) : hdr

----------------------------------------------------------------

processGET :: Request -> FilePath -> Bool -> (Maybe FilePath) -> Rsp
processGET req file ishtml rfile = runAny [
    tryGet req file ishtml langs
  , tryRedirect req rfile langs
  , just notFound
  ]
  where
    langs = map ('.':) (languages req) ++ ["",".en"]

tryGet :: Request -> FilePath -> Bool -> [String] -> MRsp
tryGet req file True langs = runAnyMaybe $ map (tryGetFile req file) langs
tryGet req file _    _     = tryGetFile req file ""

tryGetFile :: Request -> FilePath -> String -> MRsp
tryGetFile req file lang = do
    let file' = if null lang then file else file ++ lang
    (liftIO $ fileInfo file') |>| \(size, mtime) -> do
      let hdr = newHeader file mtime
          mst = ifmodified req size mtime
            ||| ifunmodified req size mtime
            ||| ifrange req size mtime
            ||| unconditional req size mtime
      case mst of
        Just st
          | st == statusOK -> just $ Ctl statusOK hdr (CtlFile file' size)
          -- FIXME skip len
          | st == statusPartialContent -> undefined
          | otherwise      -> just $ Ctl st hdr CtlNone
        _                  -> nothing -- never reached

----------------------------------------------------------------

processHEAD :: Request -> FilePath -> Bool -> (Maybe FilePath) -> Rsp
processHEAD req file ishtml rfile = runAny [
    tryHead req file ishtml langs
  , tryRedirect req rfile langs
  , just notFound
  ]
  where
    langs = map ('.':) (languages req) ++ ["",".en"]

tryHead :: Request -> FilePath -> Bool -> [String] -> MRsp
tryHead req file True langs = runAnyMaybe $ map (tryHeadFile req file) langs
tryHead req file _    _     = tryHeadFile req file ""

tryHeadFile :: Request -> FilePath -> String -> MRsp
tryHeadFile req file lang = do
    let file' = if null lang then file else file ++ lang
    (liftIO $ fileInfo file') |>| \(size, mtime) -> do
      let hdr = newHeader file mtime
          mst = ifmodified req size mtime
            ||| Just statusOK
      case mst of
        Just st -> just $ Ctl st hdr CtlNone
        _       -> nothing -- never reached

----------------------------------------------------------------

tryRedirect  :: Request -> (Maybe FilePath) -> [String] -> MRsp
tryRedirect _   Nothing     _     = nothing
tryRedirect req (Just file) langs =
    runAnyMaybe $ map (tryRedirectFile req file) langs

tryRedirectFile :: Request -> FilePath -> String -> MRsp
tryRedirectFile req file lang = do
    let file' = file ++ lang
    minfo <- liftIO $ fileInfo file'
    case minfo of
      Nothing -> nothing
      Just _  -> just $ Ctl statusMovedPermanently hdr CtlNone
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

notFound :: Ctl
notFound = Ctl statusNotFound textPlain (CtlBody "Not Found")

notAllowed :: Ctl
notAllowed = Ctl statusNotAllowed textPlain (CtlBody "Method Not Allowed")
