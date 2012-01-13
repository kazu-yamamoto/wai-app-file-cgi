{-# LANGUAGE OverloadedStrings, CPP, DoAndIfThenElse #-}

module Network.Wai.Application.Classic.CGI (
    cgiApp
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (unpack)
import qualified Data.ByteString.Char8 as BS (readInt, unpack)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Conduit
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types
import Network.Wai.Logger.Utils
import Prelude hiding (catch)
import System.IO
import System.Process

----------------------------------------------------------------

type ENVVARS = [(String,String)]

gatewayInterface :: String
gatewayInterface = "CGI/1.1"

----------------------------------------------------------------

{-|
  Handle GET and POST for CGI.

The program to link this library must ignore SIGCHLD as follows:

>   installHandler sigCHLD Ignore Nothing
-}
cgiApp :: ClassicAppSpec -> CgiRoute -> Application
cgiApp cspec cgii req = case method of
    "GET"  -> cgiApp' False cspec cgii req
    "POST" -> cgiApp' True  cspec cgii req
    _      -> return $ responseLBS statusNotAllowed textPlainHeader "Method Not Allowed\r\n" -- xxx
  where
    method = requestMethod req

cgiApp' :: Bool -> ClassicAppSpec -> CgiRoute -> Application
cgiApp' body cspec cgii req = do
    (rhdl,whdl,pid) <- liftIO $ execProcess cspec cgii req
    register $ do
        hClose whdl
        hClose rhdl
        terminateProcess pid -- SIGTERM
    requestBody req $$ toCGI whdl body
    liftIO $ hClose whdl
    fromCGI rhdl cspec req

----------------------------------------------------------------

-- FIXME sinkHandle
toCGI :: Handle -> Bool -> Sink ByteString IO ()
toCGI whdl body = when body tocgi
  where
    tocgi = do
        m <- CL.head
        case m of
            Nothing -> return ()
            Just b  -> liftIO (BS.hPutStr whdl b) >> tocgi

fromCGI :: Handle -> ClassicAppSpec -> Application
fromCGI rhdl cspec req = do
    bsrc <- bufferSource $ CB.sourceHandle rhdl
    m <- check <$> (bsrc $$ parseHeader)
    let (st, hdr, hasBody) = case m of
            Nothing    -> (statusServerError,[],False)
            Just (s,h) -> (s,h,True)
        hdr' = addServer cspec hdr
    liftIO $ print (st, hdr, hasBody)
    liftIO $ logger cspec req st Nothing
    if hasBody then
        return $ ResponseSource st hdr' (toSource bsrc)
    else
        return $ ResponseSource st hdr' (nullSource bsrc)
  where
    check hs = lookup fkContentType hs >> case lookup "status" hs of
        Nothing -> Just (status200, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        hs' = filter (\(k,_) -> k /= "status") hs
    toStatus s = BS.readInt s >>= \x -> Just (Status (fst x) s)

----------------------------------------------------------------

execProcess :: ClassicAppSpec -> CgiRoute -> Request -> IO (Handle, Handle, ProcessHandle)
execProcess cspec cgii req = do
    let naddr = showSockAddr . remoteHost $ req
    (Just whdl,Just rhdl,_,pid) <- createProcess . proSpec $ naddr
    hSetEncoding rhdl latin1
    hSetEncoding whdl latin1
    return (rhdl, whdl, pid)
 where
    proSpec naddr = CreateProcess {
        cmdspec = RawCommand prog []
      , cwd = Nothing
      , env = Just (makeEnv req naddr scriptName pathinfo (softwareName cspec))
      , std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
#if __GLASGOW_HASKELL__ >= 702
      , create_group = True
#endif
      }
    (prog, scriptName, pathinfo) = pathinfoToCGI (cgiSrc cgii)
                                                 (cgiDst cgii)
                                                 (fromByteString (rawPathInfo req))

makeEnv :: Request -> NumericAddress -> String -> String -> ByteString -> ENVVARS
makeEnv req naddr scriptName pathinfo sname = addLen . addType . addCookie $ baseEnv
  where
    baseEnv = [
        ("GATEWAY_INTERFACE", gatewayInterface)
      , ("SCRIPT_NAME",       scriptName)
      , ("REQUEST_METHOD",    BS.unpack . requestMethod $ req)
      , ("SERVER_NAME",       BS.unpack . serverName $ req)
      , ("SERVER_PORT",       show . serverPort $ req)
      , ("REMOTE_ADDR",       naddr)
      , ("SERVER_PROTOCOL",   show . httpVersion $ req)
      , ("SERVER_SOFTWARE",   BS.unpack sname)
      , ("PATH_INFO",         pathinfo)
      , ("QUERY_STRING",      query req)
      ]
    headers = requestHeaders req
    addLen = addEnv "CONTENT_LENGTH" $ lookup fkContentLength headers
    addType   = addEnv "CONTENT_TYPE" $ lookup fkContentType headers
    addCookie = addEnv "HTTP_COOKIE" $ lookup fkCookie headers
    query = BS.unpack . safeTail . rawQueryString
      where
        safeTail "" = ""
        safeTail bs = BS.tail bs

addEnv :: String -> Maybe ByteString -> ENVVARS -> ENVVARS
addEnv _   Nothing    envs = envs
addEnv key (Just val) envs = (key,BS.unpack val) : envs

pathinfoToCGI :: Path -> Path -> Path -> (FilePath, String, String)
pathinfoToCGI src dst path = (prog, scriptName, pathinfo)
  where
    path' = path <\> src
    (prog',pathinfo') = breakAtSeparator path'
    prog = pathString (dst </> prog')
    scriptName = pathString (src </> prog')
    pathinfo = pathString pathinfo'
