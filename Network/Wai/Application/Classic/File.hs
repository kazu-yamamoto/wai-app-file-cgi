{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.File (
    fileApp
  , redirectHeader
  ) where

import Control.Applicative
import Control.Exception.IOChoice.Lifted
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
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

----------------------------------------------------------------

type Rsp = IO RspSpec

----------------------------------------------------------------

data HandlerInfo = HandlerInfo FileAppSpec Request Path [Lang]

langSuffixes :: Request -> [Lang]
langSuffixes req = map (\x -> (<.> x)) langs ++ [id, (<.> "en")]
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
        Right GET  -> processGET  hinfo ishtml rfile
        Right HEAD -> processHEAD hinfo ishtml rfile
        _          -> return notAllowed
    (response, mlen) <- case body of
            NoBody                 -> noBody st
            BodyStatus             -> bodyStatus st
            BodyFileNoBody hdr     -> bodyFileNoBody st hdr
            BodyFile hdr afile rng -> bodyFile st hdr afile rng
    liftIO $ logger cspec req st mlen
    return response
  where
    hinfo = HandlerInfo spec req file langs
    method = parseMethod $ requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    langs = langSuffixes req
    zdater = dater cspec
    noBody st = do
        hdr <- liftIO . addDate zdater $ addServer cspec []
        return (responseLBS st hdr "", Nothing)
    bodyStatus st = liftIO (getStatusInfo cspec spec langs st)
                >>= statusBody st
    statusBody st StatusNone = noBody st
    statusBody st (StatusByteString bd) = do
        hdr <- liftIO . addDate zdater $ addServer cspec textPlainHeader
        return (responseLBS st hdr bd, Just (len bd))
      where
        len = fromIntegral . BL.length
    statusBody st (StatusFile afile len) = do
        hdr <- liftIO . addDate zdater $ addServer cspec textHtmlHeader
        return (ResponseFile st hdr fl mfp, Just len)
      where
        mfp = Just (FilePart 0 len len)
        fl = pathString afile
    bodyFileNoBody st hdr = do
        hdr' <- liftIO . addDate zdater $ addServer cspec hdr
        return (responseLBS st hdr' "", Nothing)
    bodyFile st hdr afile rng = do
        hdr' <- liftIO . addDate zdater $ addServer cspec hdr
        return (ResponseFile st hdr' fl mfp, Just len)
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
tryGet hinfo@(HandlerInfo _ _ _ langs) True =
    runAnyOne $ map (tryGetFile hinfo True) langs
tryGet hinfo False = tryGetFile hinfo False id

tryGetFile :: HandlerInfo -> Bool -> Lang -> Rsp
tryGetFile (HandlerInfo spec req file _) ishtml lang = do
    finfo <- liftIO $ getFileInfo spec (lang file)
    let mtime = fileInfoTime finfo
        size = fileInfoSize finfo
        sfile = fileInfoName finfo
        date = fileInfoDate finfo
        hdr = newHeader ishtml (pathByteString file) date
        Just pst = ifmodified req size mtime -- never Nothing
               <|> ifunmodified req size mtime
               <|> ifrange req size mtime
               <|> unconditional req size mtime
    case pst of
        Full st
          | st == ok200  -> return $ RspSpec ok200 (BodyFile hdr sfile (Entire size))
          | otherwise    -> return $ RspSpec st (BodyFileNoBody hdr)
        Partial skip len -> return $ RspSpec partialContent206 (BodyFile hdr sfile (Part skip len size))

----------------------------------------------------------------

processHEAD :: HandlerInfo -> Bool -> Maybe Path -> Rsp
processHEAD hinfo ishtml rfile = tryHead     hinfo ishtml
                             ||> tryRedirect hinfo rfile
                             ||> return notFoundNoBody

tryHead :: HandlerInfo -> Bool -> Rsp
tryHead hinfo@(HandlerInfo _ _ _ langs) True =
    runAnyOne $ map (tryHeadFile hinfo True) langs
tryHead hinfo False= tryHeadFile hinfo False id

tryHeadFile :: HandlerInfo -> Bool -> Lang -> Rsp
tryHeadFile (HandlerInfo spec req file _) ishtml lang = do
    finfo <- liftIO $ getFileInfo spec (lang file)
    let mtime = fileInfoTime finfo
        size = fileInfoSize finfo
        date = fileInfoDate finfo
        hdr = newHeader ishtml (pathByteString file) date
        Just pst = ifmodified req size mtime -- never Nothing
               <|> Just (Full ok200)
    case pst of
        Full st -> return $ RspSpec st (BodyFileNoBody hdr)
        _       -> goNext -- never reached

----------------------------------------------------------------

tryRedirect  :: HandlerInfo -> Maybe Path -> Rsp
tryRedirect _ Nothing = goNext
tryRedirect (HandlerInfo spec req _ langs) (Just file) =
    runAnyOne $ map (tryRedirectFile hinfo) langs
  where
    hinfo = HandlerInfo spec req file langs

tryRedirectFile :: HandlerInfo -> Lang -> Rsp
tryRedirectFile (HandlerInfo spec req file _) lang = do
    _ <- liftIO $ getFileInfo spec (lang file)
    return $ RspSpec movedPermanently301 (BodyFileNoBody hdr)
  where
    hdr = redirectHeader req

redirectHeader :: Request -> ResponseHeaders
redirectHeader = locationHeader . redirectURL

redirectURL :: Request -> ByteString
redirectURL req = BS.concat [
    "http://"
  -- Host includes ":<port>" if it is not 80.
  , host
  , rawPathInfo req
  , "/"
  ]
  where
    host = lookupRequestField' hHost req

----------------------------------------------------------------

notFound :: RspSpec
notFound = RspSpec notFound404 BodyStatus

notFoundNoBody :: RspSpec
notFoundNoBody = RspSpec notFound404 NoBody

notAllowed :: RspSpec
notAllowed = RspSpec methodNotAllowed405 BodyStatus
