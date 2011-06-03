{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.File (
    fileApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (unpack, pack)
import qualified Data.ByteString.Char8 as BS (unpack, pack)
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

fileApp :: AppSpec -> FileRoute -> Application
fileApp spec filei req = do
    RspSpec st hdr body <- case method of
        "GET"  -> processGET  req file ishtml rfile
        "HEAD" -> processHEAD req file ishtml rfile
        _      -> return notAllowed
    let hdr'= addServer hdr
        (response, mlen) = case body of
            NoBody     -> (responseLBS st hdr' "", Nothing)
            BodyLBS bd ->
                let len = fromIntegral $ BL.length bd
                in (responseLBS st hdr' bd, Just len)
            BodyFile afile rng ->
                let (len, mfp) = case rng of
                        Entire bytes    -> (bytes, Nothing)
                        Part skip bytes -> (bytes, Just (FilePart skip bytes))
                    hdr''  = addLength hdr' len
                    afile' = BS.unpack afile
                in (ResponseFile st hdr'' afile' mfp, Just len)
    liftIO $ logger spec req st mlen
    return response
  where
    method = requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    addServer hdr = ("Server", softwareName spec) : hdr
    addLength hdr len = ("Content-Length", BS.pack . show $ len) : hdr

----------------------------------------------------------------

type Lang = Maybe ByteString

langSuffixes :: Request -> [Lang]
langSuffixes req = map (Just . BS.cons 46) (languages req) ++ [Nothing, Just ".en"] -- '.'

----------------------------------------------------------------

processGET :: Request -> ByteString -> Bool -> Maybe ByteString -> Rsp
processGET req file ishtml rfile = runAny [
    tryGet req file ishtml
  , tryRedirect req rfile
  , just notFound
  ]

tryGet :: Request -> ByteString -> Bool -> MRsp
tryGet req file True  = runAnyMaybe $ map (tryGetFile req file True) langs
  where
    langs = langSuffixes req
tryGet req file False = tryGetFile req file False Nothing

tryGetFile :: Request -> ByteString -> Bool -> Lang -> MRsp
tryGetFile req file ishtml mlang = do
    let file' = maybe file (file +++) mlang
    liftIO (fileInfo file') |>| \(size, mtime) -> do
      let hdr = newHeader ishtml file mtime
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
    tryHead req file ishtml
  , tryRedirect req rfile
  , just notFound
  ]

tryHead :: Request -> ByteString -> Bool -> MRsp
tryHead req file True  = runAnyMaybe $ map (tryHeadFile req file True) langs
  where
    langs = langSuffixes req
tryHead req file False= tryHeadFile req file False Nothing

tryHeadFile :: Request -> ByteString -> Bool -> Lang -> MRsp
tryHeadFile req file ishtml mlang = do
    let file' = maybe file (file +++) mlang
    liftIO (fileInfo file') |>| \(size, mtime) -> do
      let hdr = newHeader ishtml file mtime
          Just pst = ifmodified req size mtime -- never Nothing
                 ||| Just (Full statusOK)
      case pst of
          Full st -> just $ RspSpec st hdr NoBody
          _       -> nothing -- never reached

----------------------------------------------------------------

tryRedirect  :: Request -> Maybe ByteString -> MRsp
tryRedirect _   Nothing     = nothing
tryRedirect req (Just file) =
    runAnyMaybe $ map (tryRedirectFile req file) langs
  where
    langs = langSuffixes req

tryRedirectFile :: Request -> ByteString -> Lang -> MRsp
tryRedirectFile req file mlang = do
    let file' = maybe file (file +++) mlang
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
