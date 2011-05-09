{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.File (
    fileApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (unpack, pack)
import qualified Data.ByteString.Char8 as BS (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BL ()
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.FileInfo
import Network.Wai.Application.Classic.MaybeIter
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils

----------------------------------------------------------------

{-|
  Handle GET and HEAD for a static file.

If 'pathInfo' ends with \'/\', 'indexFile' is automatically
added. In this case, "Acceptable-Language:" is also handled.  Suppose
'indexFile' is "index.html" and if the value is "ja,en", then
\"index.html.ja\", \"index.html.en\", and \"index.html\" are tried to be
opened in order.

If 'pathInfo' does not end with \'/\' and a corresponding index file
exist, redirection is specified in HTTP response.

Directory contents are NOT automatically listed. To list directory
contents, an index file must be created beforehand.

The following HTTP headers are handled: Acceptable-Language:,
If-Modified-Since:, Range:, If-Range:, If-Unmodified-Since:.
-}

fileApp :: AppSpec -> FileRoute -> Application
fileApp spec filei req = do
    RspSpec st hdr body <- case method of
        "GET"  -> processGET  req file ishtml rfile
        "HEAD" -> processHEAD req file ishtml rfile
        _      -> return notAllowed
    liftIO $ logger spec req st body
    let hdr' = addServer hdr
    case body of
        NoBody     -> return $ responseLBS st hdr' ""
        BodyLBS bd -> return $ responseLBS st hdr' bd
        BodyFile afile (Entire len) -> do
            let hdr'' = addLength hdr' len
            return $ ResponseFile st hdr'' (BS.unpack afile) Nothing
        BodyFile afile (Part skip len) -> do
            let hdr'' = addLength hdr' len
            return $ ResponseFile st hdr'' (BS.unpack afile) (Just (FilePart skip len))
  where
    method = requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    addServer hdr = ("Server", softwareName spec) : hdr
    addLength hdr len = ("Content-Length", BS.pack . show $ len) : hdr

----------------------------------------------------------------

processGET :: Request -> ByteString -> Bool -> Maybe ByteString -> Rsp
processGET req file ishtml rfile = runAny [
    tryGet req file ishtml langs
  , tryRedirect req rfile langs
  , just notFound
  ]
  where
    langs = map (46 `BS.cons`) (languages req) ++ ["",".en"] -- '.'

tryGet :: Request -> ByteString -> Bool -> [ByteString] -> MRsp
tryGet req file True langs = runAnyMaybe $ map (tryGetFile req file) langs
tryGet req file _    _     = tryGetFile req file ""

tryGetFile :: Request -> ByteString -> ByteString -> MRsp
tryGetFile req file lang = do
    let file' = if BS.null lang then file else file +++ lang
    liftIO (fileInfo file') |>| \(size, mtime) -> do
      let hdr = newHeader file mtime
          Just pst = ifmodified req size mtime -- never Nothing
                 ||| ifunmodified req size mtime
                 ||| ifrange req size mtime
                 ||| unconditional req size mtime
      case pst of
          Full st
            | st == statusOK -> just $ RspSpec statusOK hdr (BodyFile file' (Entire size))
            | otherwise      -> just $ RspSpec st hdr NoBody

          Partial skip len   -> just $ RspSpec statusPartialContent hdr (BodyFile file' (Part skip len))

----------------------------------------------------------------

processHEAD :: Request -> ByteString -> Bool -> Maybe ByteString -> Rsp
processHEAD req file ishtml rfile = runAny [
    tryHead req file ishtml langs
  , tryRedirect req rfile langs
  , just notFound
  ]
  where
    langs = map (46 `BS.cons`) (languages req) ++ ["",".en"]

tryHead :: Request -> ByteString -> Bool -> [ByteString] -> MRsp
tryHead req file True langs = runAnyMaybe $ map (tryHeadFile req file) langs
tryHead req file _    _     = tryHeadFile req file ""

tryHeadFile :: Request -> ByteString -> ByteString -> MRsp
tryHeadFile req file lang = do
    let file' = if BS.null lang then file else file +++ lang
    liftIO (fileInfo file') |>| \(size, mtime) -> do
      let hdr = newHeader file mtime
          Just pst = ifmodified req size mtime -- never Nothing
                 ||| Just (Full statusOK)
      case pst of
          Full st -> just $ RspSpec st hdr NoBody
          _       -> nothing -- never reached

----------------------------------------------------------------

tryRedirect  :: Request -> Maybe ByteString -> [ByteString] -> MRsp
tryRedirect _   Nothing     _     = nothing
tryRedirect req (Just file) langs =
    runAnyMaybe $ map (tryRedirectFile req file) langs

tryRedirectFile :: Request -> ByteString -> ByteString -> MRsp
tryRedirectFile req file lang = do
    let file' = file +++ lang
    minfo <- liftIO $ fileInfo file'
    case minfo of
      Nothing -> nothing
      Just _  -> just $ RspSpec statusMovedPermanently hdr NoBody
  where
    hdr = [("Location", redirectURL)]
    redirectURL = "http://"
              +++ serverName req
              +++ ":"
              +++ (BS.pack . show . serverPort) req
              +++ rawPathInfo req
              +++ "/"

----------------------------------------------------------------

notFound :: RspSpec
notFound = RspSpec statusNotFound textPlain (BodyLBS "Not Found\r\n")

notAllowed :: RspSpec
notAllowed = RspSpec statusNotAllowed textPlain (BodyLBS "Method Not Allowed\r\n")
