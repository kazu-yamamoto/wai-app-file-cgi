{-# LANGUAGE OverloadedStrings, CPP, ScopedTypeVariables #-}

module Network.Wai.Application.Classic.CGI (
    cgiApp
  ) where

import qualified Control.Exception as E (SomeException, IOException, try, catch, bracket)
import Control.Monad (when, (<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (readInt, unpack, tail)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Network.HTTP.Types
import Network.SockAddr
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Application.Classic.Conduit
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types
import System.Environment
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
cgiApp :: ClassicAppSpec -> CgiAppSpec -> CgiRoute -> Application
cgiApp cspec spec cgii req respond = case method of
    Right GET  -> cgiApp' False cspec spec cgii req respond
    Right POST -> cgiApp' True  cspec spec cgii req respond
    _          -> respond $ responseLBS methodNotAllowed405 textPlainHeader "Method Not Allowed\r\n" -- xxx
  where
    method = parseMethod $ requestMethod req

cgiApp' :: Bool -> ClassicAppSpec -> CgiAppSpec -> CgiRoute -> Application
cgiApp' body cspec spec cgii req respond = E.bracket setup teardown (respond <=< cgi)
  where
    setup = execProcess cspec spec cgii req
    teardown (rhdl,whdl,pid) = do
        terminateProcess pid -- SIGTERM
        hClose rhdl
        hClose whdl
    cgi (rhdl,whdl,_) = do
        when body $ toCGI whdl req
        hClose whdl -- telling EOF
        fromCGI rhdl

----------------------------------------------------------------

type TRYPATH = Either E.IOException String

toCGI :: Handle -> Request -> IO ()
toCGI whdl req = sourceRequestBody req $$ CB.sinkHandle whdl

fromCGI :: Handle -> IO Response
fromCGI rhdl = do
    (src', hs) <- cgiHeader `E.catch` recover
    let (st, hdr, hasBody) = case check hs of
            Nothing    -> (internalServerError500,[],False)
            Just (s,h) -> (s,h,True)
    let src | hasBody   = src'
            | otherwise = CL.sourceNull
    return $ responseSource st hdr src
  where
    check hs = lookup hContentType hs >> case lookup hStatus hs of
        Nothing -> Just (ok200, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        hs' = filter (\(k,_) -> k /= hStatus) hs
    toStatus s = BS.readInt s >>= \x -> Just (Status (fst x) s)
    emptyHeader = []
    recover (_ :: E.SomeException) = return (CL.sourceNull, emptyHeader)
    cgiHeader = do
        (rsrc,hs) <- CB.sourceHandle rhdl $$+ parseHeader
        src <- toResponseSource rsrc
        return (src,hs)

----------------------------------------------------------------

execProcess :: ClassicAppSpec -> CgiAppSpec -> CgiRoute -> Request -> IO (Handle, Handle, ProcessHandle)
execProcess cspec spec cgii req = do
    let naddr = showSockAddr . remoteHost $ req
    epath <- E.try (getEnv "PATH") :: IO TRYPATH
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
#if __GLASGOW_HASKELL__ >= 707
      , delegate_ctlc = False
#endif
      }
    (prog, scriptName, pathinfo) =
        pathinfoToCGI (cgiSrc cgii)
                      (cgiDst cgii)
                      (rawPathInfo req)
                      (indexCgi spec)

makeEnv :: Request -> String -> String -> String -> ByteString ->
           TRYPATH -> ENVVARS
makeEnv req naddr scriptName pathinfo sname epath = addPath epath . addLen . addType . addCookie $ baseEnv
  where
    baseEnv = [
        ("GATEWAY_INTERFACE", gatewayInterface)
      , ("SCRIPT_NAME",       scriptName)
      , ("REQUEST_METHOD",    BS.unpack . requestMethod $ req)
      , ("SERVER_NAME",       BS.unpack host)
      , ("SERVER_PORT",       BS.unpack port)
      , ("REMOTE_ADDR",       naddr)
      , ("SERVER_PROTOCOL",   show . httpVersion $ req)
      , ("SERVER_SOFTWARE",   BS.unpack sname)
      , ("PATH_INFO",         pathinfo)
      , ("QUERY_STRING",      query req)
      ]
    headers = requestHeaders req
    addLen    = addLength "CONTENT_LENGTH" $ requestBodyLength req
    addType   = addEnv "CONTENT_TYPE"   $ lookup hContentType   headers
    addCookie = addEnv "HTTP_COOKIE"    $ lookup hCookie        headers
    addPath (Left _)     ev = ev
    addPath (Right path) ev = ("PATH", path) : ev
    query = BS.unpack . safeTail . rawQueryString
      where
        safeTail "" = ""
        safeTail bs = BS.tail bs
    (host, port) = hostPort req

addEnv :: String -> Maybe ByteString -> ENVVARS -> ENVVARS
addEnv _   Nothing    envs = envs
addEnv key (Just val) envs = (key,BS.unpack val) : envs

addLength :: String -> RequestBodyLength -> ENVVARS -> ENVVARS
addLength _   ChunkedBody       envs = envs
addLength key (KnownLength len) envs = (key, show len) : envs

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
