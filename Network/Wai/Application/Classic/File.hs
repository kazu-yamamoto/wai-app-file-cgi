{-# LANGUAGE OverloadedStrings, CPP #-}

module Network.Wai.Application.Classic.File (
    fileApp
  , redirectHeader
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Exception.IOChoice
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS (concat)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.FileInfo
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Status
import Network.Wai.Application.Classic.Types
import Network.Wai.Handler.Warp (getFileInfo)

----------------------------------------------------------------

data RspSpec = NoBody    Status
             | NoBodyHdr Status ResponseHeaders
             | BodyFile  Status ResponseHeaders FilePath
             deriving (Eq,Show)

----------------------------------------------------------------

data HandlerInfo = HandlerInfo FileAppSpec Request Path [Lang]

langSuffixes :: RequestHeaders -> [Lang]
langSuffixes hdr = langs ++ ["", "en"]
  where
    langs = languages hdr

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
    rspspec <- case method of
        Right GET  -> processGET  hinfo ishtml rfile
        Right HEAD -> processGET  hinfo ishtml rfile
        _          -> return notAllowed
    response <- case rspspec of
            NoBody    st        -> bodyStatus st
            NoBodyHdr st hdr    -> return $ responseLBS st hdr ""
            BodyFile  st hdr fl -> return $ ResponseFile st hdr fl Nothing
    respond response
  where
    hinfo = HandlerInfo spec req file langs
    method = parseMethod $ requestMethod req
    path = pathinfoToFilePath req filei
    file = addIndex spec path
    ishtml = isHTML spec file
    rfile = redirectPath spec path
    langs = langSuffixes $ requestHeaders req
    noBody st = return $ responseLBS st [] ""
    bodyStatus st = liftIO (getStatusInfo cspec req langs st)
                >>= statusBody st
    statusBody st StatusNone = noBody st
    statusBody st (StatusByteString bd) =
        return $ responseLBS st hdr bd
      where
        hdr = textPlainHeader
    statusBody st (StatusFile afile len) =
        return $ ResponseFile st hdr fl mfp
      where
        mfp = Just (FilePart 0 len len)
        fl = pathString afile
        hdr = textHtmlHeader

----------------------------------------------------------------

processGET :: HandlerInfo -> Bool -> Maybe Path -> IO RspSpec
processGET hinfo ishtml rfile = tryGet      hinfo ishtml
                            ||> tryRedirect hinfo rfile
                            ||> return notFound

tryGet :: HandlerInfo -> Bool -> IO RspSpec
tryGet hinfo@(HandlerInfo _ _ _ langs) True =
    runAnyOne $ map (tryGetFile hinfo True) langs
tryGet hinfo False = tryGetFile hinfo False ""

tryGetFile :: HandlerInfo -> Bool -> Lang -> IO RspSpec
tryGetFile (HandlerInfo _ req file _) ishtml lang = do
    let file' = pathString $ file <.> lang
        hdr = newHeader ishtml file
    _ <- liftIO (getFileInfo req file') -- expecting an error
    return $ BodyFile ok200 hdr file'

----------------------------------------------------------------

tryRedirect  :: HandlerInfo -> Maybe Path -> IO RspSpec
tryRedirect _ Nothing = goNext
tryRedirect (HandlerInfo spec req _ langs) (Just file) =
    runAnyOne $ map (tryRedirectFile hinfo) langs
  where
    hinfo = HandlerInfo spec req file langs

tryRedirectFile :: HandlerInfo -> Lang -> IO RspSpec
tryRedirectFile (HandlerInfo _ req file _) lang = do
    let file' = pathString $ file <.> lang
    _ <- liftIO $ getFileInfo req file' -- expecting an error
    return $ NoBodyHdr movedPermanently301 hdr
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
notFound = NoBody notFound404

notAllowed :: RspSpec
notAllowed = NoBody methodNotAllowed405
