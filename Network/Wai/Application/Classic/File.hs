{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.File (
    fileApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (unpack, pack)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy.Char8 as BL (length)
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

fileApp :: ClassicAppSpec -> FileAppSpec -> FileRoute -> Application
fileApp cspec spec filei req = do
    RspSpec st hdr body <- case method of
        "GET"  -> processGET  spec req file ishtml rfile
        "HEAD" -> processHEAD spec req file ishtml rfile
        _      -> return notAllowed
    let hdr'= addServer hdr
        (response, mlen) = case body of
            NoBody     -> (responseLBS st hdr' "", Nothing)
            BodyLBS bd ->
                let len = fromIntegral $ BL.length bd
                in (responseLBS st hdr' bd, Just len)
            BodyFile afile rng ->
                let (len, mfp) = case rng of
                        -- sendfile of Linux does not support the entire file
                        Entire bytes    -> (bytes, Just (FilePart 0 bytes))
                        Part skip bytes -> (bytes, Just (FilePart skip bytes))
                    hdr''  = addLength hdr' len
                in (ResponseFile st hdr'' afile mfp, Just len)
    liftIO $ logger cspec req st mlen
    return response
  where
    method = requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    addServer hdr = ("Server", softwareName cspec) : hdr
    addLength hdr len = ("Content-Length", BS.pack . show $ len) : hdr

----------------------------------------------------------------

type Lang = Maybe ByteString

langSuffixes :: Request -> [Lang]
langSuffixes req = map (Just . BS.cons 46) (languages req) ++ [Nothing, Just ".en"] -- '.'

----------------------------------------------------------------

processGET :: FileAppSpec -> Request -> ByteString -> Bool -> Maybe ByteString -> Rsp
processGET spec req file ishtml rfile = runAny [
    tryGet spec req file ishtml
  , tryRedirect spec req rfile
  , just notFound
  ]

tryGet :: FileAppSpec -> Request -> ByteString -> Bool -> MRsp
tryGet spec req file True  = runAnyMaybe $ map (tryGetFile spec req file True) langs
  where
    langs = langSuffixes req
tryGet spec req file False = tryGetFile spec req file False Nothing

tryGetFile :: FileAppSpec -> Request -> ByteString -> Bool -> Lang -> MRsp
tryGetFile spec req file ishtml mlang = do
    let file' = maybe file (file +++) mlang
    liftIO (getFileInfo spec file') |>| \finfo -> do
      let mtime = fileInfoTime finfo
          size = fileInfoSize finfo
          sfile = fileInfoName finfo
          hdr = newHeader ishtml file mtime
          Just pst = ifmodified req size mtime -- never Nothing
                 ||| ifunmodified req size mtime
                 ||| ifrange req size mtime
                 ||| unconditional req size mtime
      case pst of
          Full st
            | st == statusOK -> just $ RspSpec statusOK hdr (BodyFile sfile (Entire size))
            | otherwise      -> just $ RspSpec st hdr NoBody

          Partial skip len   -> just $ RspSpec statusPartialContent hdr (BodyFile sfile (Part skip len))

----------------------------------------------------------------

processHEAD :: FileAppSpec -> Request -> ByteString -> Bool -> Maybe ByteString -> Rsp
processHEAD spec req file ishtml rfile = runAny [
    tryHead spec req file ishtml
  , tryRedirect spec req rfile
  , just notFound
  ]

tryHead :: FileAppSpec -> Request -> ByteString -> Bool -> MRsp
tryHead spec req file True  = runAnyMaybe $ map (tryHeadFile spec req file True) langs
  where
    langs = langSuffixes req
tryHead spec req file False= tryHeadFile spec req file False Nothing

tryHeadFile :: FileAppSpec -> Request -> ByteString -> Bool -> Lang -> MRsp
tryHeadFile spec req file ishtml mlang = do
    let file' = maybe file (file +++) mlang
    liftIO (getFileInfo spec file') |>| \finfo -> do
      let mtime = fileInfoTime finfo
          size = fileInfoSize finfo
          hdr = newHeader ishtml file mtime
          Just pst = ifmodified req size mtime -- never Nothing
                 ||| Just (Full statusOK)
      case pst of
          Full st -> just $ RspSpec st hdr NoBody
          _       -> nothing -- never reached

----------------------------------------------------------------

tryRedirect  :: FileAppSpec -> Request -> Maybe ByteString -> MRsp
tryRedirect _ _ Nothing = nothing
tryRedirect spec req (Just file) =
    runAnyMaybe $ map (tryRedirectFile spec req file) langs
  where
    langs = langSuffixes req

tryRedirectFile :: FileAppSpec -> Request -> ByteString -> Lang -> MRsp
tryRedirectFile spec req file mlang = do
    let file' = maybe file (file +++) mlang
    minfo <- liftIO $ getFileInfo spec file'
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
