{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.File (
    fileApp
  , redirectHeader
  ) where

import Control.Applicative
import Control.Exception.IOChoice.Lifted
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS (concat)
import qualified Data.ByteString.Lazy.Char8 as BL (length)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.FileInfo
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Status
import Network.Wai.Application.Classic.Types
import Network.Wai.Handler.Warp (getFileInfo)

----------------------------------------------------------------

type Rsp = IO RspSpec

----------------------------------------------------------------

data HandlerInfo = HandlerInfo FileAppSpec Request IndexedHeader Path [Lang]

langSuffixes :: IndexedHeader -> [Lang]
langSuffixes reqidx = map (\x -> (<.> x)) langs ++ [id, (<.> "en")]
  where
    langs = map fromByteString $ languages reqidx

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
fileApp cspec spec filei req respond = do
    RspSpec st body <- case method of
        Right GET  -> processGET  hinfo ishtml rfile
        Right HEAD -> processGET  hinfo ishtml rfile
        _          -> return notAllowed
    (response, mlen) <- case body of
            NoBody                 -> noBody st
            BodyStatus             -> bodyStatus st
            BodyFileNoBody hdr     -> bodyFileNoBody st hdr
            BodyFile hdr afile rng -> bodyFile st hdr afile rng
    logger cspec req st mlen
    respond response
  where
    reqidx = indexRequestHeader (requestHeaders req)
    hinfo = HandlerInfo spec req reqidx file langs
    method = parseMethod $ requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    langs = langSuffixes reqidx
    noBody st = return (responseLBS st [] "", Nothing)
    bodyStatus st = liftIO (getStatusInfo cspec req langs st)
                >>= statusBody st
    statusBody st StatusNone = noBody st
    statusBody st (StatusByteString bd) =
        return (responseLBS st hdr bd, Just (len bd))
      where
        len = fromIntegral . BL.length
        hdr = textPlainHeader
    statusBody st (StatusFile afile len) =
        return (ResponseFile st hdr fl mfp, Just len)
      where
        mfp = Just (FilePart 0 len len)
        fl = pathString afile
        hdr = textHtmlHeader
    bodyFileNoBody st hdr =
        return (responseLBS st hdr "", Nothing)
    bodyFile st hdr afile rng =
        return (ResponseFile st hdr fl mfp, Just len)
      where
        (len, mfp) = case rng of
            -- sendfile of Linux does not support the entire file
            Entire bytes          -> (bytes, Just (FilePart 0 bytes bytes))
            Part skip bytes total -> (bytes, Just (FilePart skip bytes total))
        fl = pathString afile

----------------------------------------------------------------

processGET :: HandlerInfo -> Bool -> Maybe Path -> Rsp
processGET hinfo ishtml rfile = tryGet      hinfo ishtml
                            ||> tryRedirect hinfo rfile
                            ||> return notFound

tryGet :: HandlerInfo -> Bool -> Rsp
tryGet hinfo@(HandlerInfo _ _ _ _ langs) True =
    runAnyOne $ map (tryGetFile hinfo True) langs
tryGet hinfo False = tryGetFile hinfo False id

tryGetFile :: HandlerInfo -> Bool -> Lang -> Rsp
tryGetFile (HandlerInfo _ req reqidx file _) ishtml lang = do
    let file' = pathString $ lang file
    finfo <- fromFileInfo <$> liftIO (getFileInfo req file')
    let mtime = fileinfoTime finfo
        size  = fileinfoSize finfo
        sfile = fileinfoName finfo
        date  = fileinfoDate finfo
        mrange = requestHeaderRange req
        hdr = newHeader ishtml (pathByteString file) date
        Just pst = ifmodified    reqidx size mtime mrange
               <|> ifunmodified  reqidx size mtime mrange
               <|> ifrange       reqidx size mtime mrange
               <|> unconditional reqidx size mtime mrange
    case pst of
        Full st
          | st == ok200  -> return $ RspSpec ok200 (BodyFile hdr sfile (Entire size))
          | otherwise    -> return $ RspSpec st (BodyFileNoBody hdr)
        Partial skip len -> return $ RspSpec partialContent206 (BodyFile hdr sfile (Part skip len size))

----------------------------------------------------------------

tryRedirect  :: HandlerInfo -> Maybe Path -> Rsp
tryRedirect _ Nothing = goNext
tryRedirect (HandlerInfo spec req reqidx _ langs) (Just file) =
    runAnyOne $ map (tryRedirectFile hinfo) langs
  where
    hinfo = HandlerInfo spec req reqidx file langs

tryRedirectFile :: HandlerInfo -> Lang -> Rsp
tryRedirectFile (HandlerInfo _ req _ file _) lang = do
    let file' = pathString $ lang file
    _ <- liftIO $ getFileInfo req file' -- expecting an error
    return $ RspSpec movedPermanently301 (BodyFileNoBody hdr)
  where
    hdr = redirectHeader req

redirectHeader :: Request -> ResponseHeaders
redirectHeader = locationHeader . redirectURL

redirectURL :: Request -> ByteString
redirectURL req = BS.concat [
  -- Scheme must not be included because of no way to tell
  -- http or https.
    "//"
  -- Host includes ":<port>" if it is not 80.
  , host
  , rawPathInfo req
  , "/"
  ]
  where
    host = fromMaybe "" $ requestHeaderHost req

----------------------------------------------------------------

notFound :: RspSpec
notFound = RspSpec notFound404 BodyStatus

notAllowed :: RspSpec
notAllowed = RspSpec methodNotAllowed405 BodyStatus
