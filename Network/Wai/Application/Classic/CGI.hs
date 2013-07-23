{-# LANGUAGE OverloadedStrings, CPP, ScopedTypeVariables #-}

module Network.Wai.Application.Classic.CGI (
    cgiApp
  ) where

import Control.Exception (SomeException, IOException, try)
import Control.Exception.Lifted as L (catch)
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
import System.Environment
import System.IO
import System.Process

cgiApp = undefined

{-
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
cgiApp :: ClassicAppSpec -> CgiAppSpec -> CgiRoute -> Application
cgiApp cspec spec cgii req = case method of
    Right GET  -> cgiApp' False cspec spec cgii req
    Right POST -> cgiApp' True  cspec spec cgii req
    _          -> return $ responseLBS methodNotAllowed405 textPlainHeader "Method Not Allowed\r\n" -- xxx
  where
    method = parseMethod $ requestMethod req

cgiApp' :: Bool -> ClassicAppSpec -> CgiAppSpec -> CgiRoute -> Application
cgiApp' body cspec spec cgii req = do
    (rhdl,whdl,tellEOF) <- liftIO (execProcess cspec spec cgii req) >>= register3
    when body $ toCGI whdl req
    tellEOF
    fromCGI rhdl cspec req
  where
    register3 (rhdl,whdl,pid) = do
        _ <- register $ terminateProcess pid -- SIGTERM
        _ <- register $ hClose rhdl
        keyw <- register $ hClose whdl
        return (rhdl,whdl,release keyw)

----------------------------------------------------------------

type TRYPATH = Either IOException String

toCGI :: Handle -> Request -> ResourceT IO ()
toCGI whdl req = requestBody req $$ CB.sinkHandle whdl

fromCGI :: Handle -> ClassicAppSpec -> Application
fromCGI rhdl cspec req = do
    (src', hs) <- cgiHeader `L.catch` recover
    let (st, hdr, hasBody) = case check hs of
            Nothing    -> (internalServerError500,[],False)
            Just (s,h) -> (s,h,True)
        hdr' = addServer cspec hdr
    liftIO $ logger cspec req st Nothing
    let src = if hasBody then src' else CL.sourceNull
    return $ ResponseSource st hdr' src
  where
    check hs = lookup hContentType hs >> case lookup hStatus hs of
        Nothing -> Just (ok200, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        hs' = filter (\(k,_) -> k /= hStatus) hs
    toStatus s = BS.readInt s >>= \x -> Just (Status (fst x) s)
    emptyHeader = []
    recover (_ :: SomeException) = return (CL.sourceNull, emptyHeader)
    cgiHeader = do
        (rsrc,hs) <- CB.sourceHandle rhdl $$+ parseHeader
        src <- toResponseSource rsrc
        return (src,hs)

----------------------------------------------------------------

execProcess :: ClassicAppSpec -> CgiAppSpec -> CgiRoute -> Request -> IO (Handle, Handle, ProcessHandle)
execProcess cspec spec cgii req = do
    let naddr = showSockAddr . remoteHost $ req
    epath <- try (getEnv "PATH") :: IO TRYPATH
    (Just whdl,Just rhdl,_,pid) <- createProcess $ proSpec naddr epath
    hSetEncoding rhdl latin1
    hSetEncoding whdl latin1
    return (rhdl, whdl, pid)
 where
    proSpec naddr epath = CreateProcess {
        cmdspec = RawCommand prog []
      , cwd = Nothing
      , env = Just $ makeEnv req naddr scriptName pathinfo (softwareName cspec) epath
      , std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
#if __GLASGOW_HASKELL__ >= 702
      , create_group = True
#endif
      }
    (prog, scriptName, pathinfo) =
        pathinfoToCGI (cgiSrc cgii)
                      (cgiDst cgii)
                      (fromByteString (rawPathInfo req))
                      (indexCgi spec)

makeEnv :: Request -> NumericAddress -> String -> String -> ByteString ->
           TRYPATH -> ENVVARS
makeEnv req naddr scriptName pathinfo sname epath = addPath epath . addLen . addType . addCookie $ baseEnv
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
    addLen    = addEnv "CONTENT_LENGTH" $ lookup hContentLength headers
    addType   = addEnv "CONTENT_TYPE"   $ lookup hContentType   headers
    addCookie = addEnv "HTTP_COOKIE"    $ lookup hCookie        headers
    addPath (Left _)     ev = ev
    addPath (Right path) ev = ("PATH", path) : ev
    query = BS.unpack . safeTail . rawQueryString
      where
        safeTail "" = ""
        safeTail bs = BS.tail bs

addEnv :: String -> Maybe ByteString -> ENVVARS -> ENVVARS
addEnv _   Nothing    envs = envs
addEnv key (Just val) envs = (key,BS.unpack val) : envs

{-|

>>> pathinfoToCGI "/cgi-bin/" "/User/cgi-bin/" "/cgi-bin/foo" "index.cgi"
("/User/cgi-bin/foo","/cgi-bin/foo","")
>>> pathinfoToCGI "/cgi-bin/" "/User/cgi-bin/" "/cgi-bin/foo/bar" "index.cgi"
("/User/cgi-bin/foo","/cgi-bin/foo","/bar")
>>> pathinfoToCGI "/cgi-bin/" "/User/cgi-bin/" "/cgi-bin/" "index.cgi"
("/User/cgi-bin/index.cgi","/cgi-bin/index.cgi","")

-}

pathinfoToCGI :: Path -> Path -> Path -> Path -> (FilePath, String, String)
pathinfoToCGI src dst path index = (prog, scriptName, pathinfo)
  where
    path' = path <\> src
    (prog',pathinfo')
        | src == path = (index, "")
        | otherwise   = breakAtSeparator path'
    prog = pathString (dst </> prog')
    scriptName = pathString (src </> prog')
    pathinfo = pathString pathinfo'
-}