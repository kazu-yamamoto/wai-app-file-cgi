{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.File (
    fileApp
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy.Char8 as BL (length)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.FileInfo
import Network.Wai.Application.Classic.MaybeIter
import Network.Wai.Application.Classic.Status
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils

----------------------------------------------------------------

data HandlerInfo = HandlerInfo FileAppSpec Request Path [Lang]

langSuffixes :: Request -> [Lang]
langSuffixes req = map (\x -> (<.> x)) langs ++ [id, (<.> ".en")]
  where
    langs = map fromByteString $ languages req

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
    RspSpec st body <- case method of
        "GET"  -> processGET  hinfo ishtml rfile
        "HEAD" -> processHEAD hinfo ishtml rfile
        _      -> return notAllowed
    (response, mlen) <- case body of
            NoBody                 -> return $ noBody st
            BodyStatus -> statusBody st <$> (liftIO $ getStatusInfo cspec spec langs st)
            BodyFileNoBody hdr     -> return $ bodyFileNoBody st hdr
            BodyFile hdr afile rng -> return $ bodyFile st hdr afile rng
    liftIO $ logger cspec req st mlen
    return response
  where
    hinfo = HandlerInfo spec req file langs
    method = requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    langs = langSuffixes req
    noBody st = (responseLBS st hdr "", Nothing)
      where
        hdr = addServer cspec []
    statusBody st StatusNone = noBody st
    statusBody st (StatusByteString bd) = (responseLBS st hdr bd, Just (len bd))
      where
        len = fromIntegral . BL.length
        hdr = addServer cspec textPlainHeader
    statusBody st (StatusFile afile len) = (ResponseFile st hdr fl mfp, Just len)
      where
        hdr = addServer cspec textHtmlHeader
        mfp = Just (FilePart 0 len)
        fl = pathString afile
    bodyFileNoBody st hdr = (responseLBS st hdr' "", Nothing)
      where
        hdr' = addServer cspec hdr
    bodyFile st hdr afile rng = (ResponseFile st hdr' fl mfp, Just len)
      where
        (len, mfp) = case rng of
            -- sendfile of Linux does not support the entire file
            Entire bytes    -> (bytes, Just (FilePart 0 bytes))
            Part skip bytes -> (bytes, Just (FilePart skip bytes))
        hdr' = addLength len $ addServer cspec hdr
        fl = pathString afile

----------------------------------------------------------------

processGET :: HandlerInfo -> Bool -> Maybe Path -> Rsp
processGET hinfo ishtml rfile = runAny [
    tryGet      hinfo ishtml
  , tryRedirect hinfo rfile
  , just notFound
  ]

tryGet :: HandlerInfo -> Bool -> MRsp
tryGet hinfo@(HandlerInfo _ _ _ langs) True =
    runAnyMaybe $ map (tryGetFile hinfo True) langs
tryGet hinfo False = tryGetFile hinfo False id

tryGetFile :: HandlerInfo -> Bool -> Lang -> MRsp
tryGetFile (HandlerInfo spec req file _) ishtml lang = do
    let file' = lang file
    liftIO (getFileInfo spec file') |>| \finfo -> do
      let mtime = fileInfoTime finfo
          size = fileInfoSize finfo
          sfile = fileInfoName finfo
          hdr = newHeader ishtml (pathByteString file) mtime
          Just pst = ifmodified req size mtime -- never Nothing
                 ||| ifunmodified req size mtime
                 ||| ifrange req size mtime
                 ||| unconditional req size mtime
      case pst of
          Full st
            | st == statusOK -> just $ RspSpec statusOK (BodyFile hdr sfile (Entire size))
            | otherwise      -> just $ RspSpec st (BodyFileNoBody hdr)

          Partial skip len   -> just $ RspSpec statusPartialContent (BodyFile hdr sfile (Part skip len))

----------------------------------------------------------------

processHEAD :: HandlerInfo -> Bool -> Maybe Path -> Rsp
processHEAD hinfo ishtml rfile = runAny [
    tryHead     hinfo ishtml
  , tryRedirect hinfo rfile
  , just notFound
  ]

tryHead :: HandlerInfo -> Bool -> MRsp
tryHead hinfo@(HandlerInfo _ _ _ langs) True =
    runAnyMaybe $ map (tryHeadFile hinfo True) langs
tryHead hinfo False= tryHeadFile hinfo False id

tryHeadFile :: HandlerInfo -> Bool -> Lang -> MRsp
tryHeadFile (HandlerInfo spec req file _) ishtml lang = do
    let file' = lang file
    liftIO (getFileInfo spec file') |>| \finfo -> do
      let mtime = fileInfoTime finfo
          size = fileInfoSize finfo
          hdr = newHeader ishtml (pathByteString file) mtime
          Just pst = ifmodified req size mtime -- never Nothing
                 ||| Just (Full statusOK)
      case pst of
          Full st -> just $ RspSpec st (BodyFileNoBody hdr)
          _       -> nothing -- never reached

----------------------------------------------------------------

tryRedirect  :: HandlerInfo -> Maybe Path -> MRsp
tryRedirect _ Nothing = nothing
tryRedirect (HandlerInfo spec req _ langs) (Just file) =
    runAnyMaybe $ map (tryRedirectFile hinfo) langs
  where
    hinfo = HandlerInfo spec req file langs

tryRedirectFile :: HandlerInfo -> Lang -> MRsp
tryRedirectFile (HandlerInfo spec req file _) lang = do
    let file' = lang file
    minfo <- liftIO $ getFileInfo spec file'
    case minfo of
      Nothing -> nothing
      Just _  -> just $ RspSpec statusMovedPermanently (BodyFileNoBody hdr)
  where
    hdr = locationHeader redirectURL
    redirectURL = concatByteString [ "http://"
                                   , serverName req
                                   , ":"
                                   , (BS.pack . show . serverPort) req
                                   , rawPathInfo req
                                   , "/"
                                   ]

----------------------------------------------------------------

notFound :: RspSpec
notFound = RspSpec statusNotFound BodyStatus

notAllowed :: RspSpec
notAllowed = RspSpec statusNotAllowed BodyStatus
