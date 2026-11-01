{-# LANGUAGE OverloadedStrings, CPP, ScopedTypeVariables #-}

module Network.Wai.Application.Classic.CGI (
    cgiApp
  ) where

import qualified Control.Exception as E (SomeException, SomeAsyncException, IOException, try, catch, bracket, fromException, throwIO)
import Control.Concurrent (forkFinally, forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (mplus, void)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (foldedCase)
import Data.Char (toUpper)
import Data.List (nub)
import qualified Data.ByteString.Char8 as BS (dropWhile, elem, intercalate, pack, readInt, unpack, tail)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Network.HTTP.Types
import Network.SockAddr
import Network.Socket (SockAddr (..))
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

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

type ENVVARS = [(String,String)]

gatewayInterface :: String
gatewayInterface = "CGI/1.1"

----------------------------------------------------------------

{-|
  Handle any methods for CGI.

CGI processes are reaped by this library. The program to link this
library should not ignore SIGCHLD. Otherwise, the PID of a finished
CGI process may be reused by another process before it is terminated.
-}
cgiApp :: ClassicAppSpec -> CgiAppSpec -> CgiRoute -> Application
cgiApp cspec spec cgii req respond
  | hasDotDotSegment (rawPathInfo req) =
      respond $ responseLBS badRequest400 textPlainHeader "Bad Request\r\n"
  | otherwise = E.bracket setup teardown cgi
  where
    setup = execProcess cspec spec cgii req
    -- An unreaped process keeps its PID, so SIGTERM cannot hit
    -- another process. Reaping may block, hence in another thread.
    teardown (rhdl,whdl,pid) = do
        terminateProcess pid -- SIGTERM
        hClose rhdl
        hClose whdl
        void $ forkIO $ void (waitForProcess pid) `E.catch` ignore
    -- The body is written in another thread so that the CGI output
    -- can be read concurrently. Otherwise, both pipes may fill up.
    cgi (rhdl,whdl,_) = E.bracket (forkWriter whdl) stopWriter $ \_ ->
        respond =<< fromCGI rhdl
    forkWriter whdl = do
        done <- newEmptyMVar
        tid <- forkFinally (toCGI whdl req) $ \_ -> do
            hClose whdl `E.catch` ignore -- telling EOF
            putMVar done ()
        return (tid, done)
    stopWriter (tid, done) = killThread tid >> takeMVar done
    ignore (_ :: E.IOException) = return ()

----------------------------------------------------------------

type TRYPATH = Either E.IOException String

toCGI :: Handle -> Request -> IO ()
#if MIN_VERSION_conduit(1,3,0)
toCGI whdl req = runConduit (sourceRequestBody req .| CB.sinkHandle whdl)
#else
toCGI whdl req = sourceRequestBody req $$ CB.sinkHandle whdl
#endif

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
    -- A redirect response may lack Content-Type (RFC 3875 Section 6.2).
    check hs = (lookup hContentType hs `mplus` location) >> case lookup hStatus hs of
        Nothing -> Just (maybe ok200 (const found302) location, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        location = lookup hLocation hs
        hs' = filter (\(k,_) -> k /= hStatus) hs
    toStatus s = BS.readInt s >>= \(code,msg) -> Just (Status code (BS.dropWhile (== ' ') msg))
    emptyHeader = []
    recover (e :: E.SomeException)
      | isAsync e = E.throwIO e
      | otherwise = return (CL.sourceNull, emptyHeader)
    isAsync e = case E.fromException e of
        Just (_ :: E.SomeAsyncException) -> True
        Nothing                          -> False
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
      , env = Just $ makeEnv req naddr prog scriptName pathinfo (softwareName cspec) epath
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
#if __GLASGOW_HASKELL__ >= 800
      , detach_console = False
      , create_new_console = False
      , new_session = False
      , child_group = Nothing
      , child_user = Nothing
#endif
#if __GLASGOW_HASKELL__ >= 802
      , use_process_jobs = False
#endif
      }
    (prog, scriptName, pathinfo) =
        pathinfoToCGI (cgiSrc cgii)
                      (cgiDst cgii)
                      (rawPathInfo req)
                      (indexCgi spec)

makeEnv :: Request -> String -> FilePath -> String -> String -> ByteString ->
           TRYPATH -> ENVVARS
makeEnv req naddr prog scriptName pathinfo sname epath =
    addPath epath . addLen . addType . addPort . addHttps $ baseEnv ++ httpEnv
  where
    baseEnv = [
        ("GATEWAY_INTERFACE", gatewayInterface)
      , ("SCRIPT_NAME",       scriptName)
      , ("SCRIPT_FILENAME",   prog)
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
    addPort   = addEnv "REMOTE_PORT"    $ remotePort $ remoteHost req
    addHttps ev | isSecure req = ("HTTPS", "on") : ev
                | otherwise    = ev
    httpEnv = httpHeadersToEnv headers
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

remotePort :: SockAddr -> Maybe ByteString
remotePort (SockAddrInet p _)       = Just $ BS.pack $ show p
remotePort (SockAddrInet6 p _ _ _)  = Just $ BS.pack $ show p
remotePort _                        = Nothing

{-|
  Converting request headers to HTTP_* variables.
  Values of the same header are joined.
  Headers containing '_' are dropped since they would collide with
  ones containing '-' (e.g. X_Forwarded_For vs X-Forwarded-For).

>>> httpHeadersToEnv [("User-Agent","foo"),("Accept","a"),("accept","b"),("Proxy","x"),("Authorization","y"),("Content-Type","z")]
[("HTTP_USER_AGENT","foo"),("HTTP_ACCEPT","a, b")]
>>> httpHeadersToEnv [("X-Forwarded-For","192.0.2.1"),("X_Forwarded_For","192.0.2.2")]
[("HTTP_X_FORWARDED_FOR","192.0.2.1")]
-}
httpHeadersToEnv :: RequestHeaders -> ENVVARS
httpHeadersToEnv hdrs = map toEnv $ nub $ map fst relayed
  where
    relayed = filter (\(k,_) -> k `notElem` excluded && not (hasUnderscore k)) hdrs
    hasUnderscore = BS.elem '_' . foldedCase
    -- "Proxy" is excluded to prevent httpoxy.
    excluded = [hContentType, hContentLength, hAuthorization, hProxyAuthorization, "proxy"]
    toEnv k = (envName k, BS.unpack $ BS.intercalate (sep k) [v | (k',v) <- relayed, k' == k])
    sep k | k == hCookie = "; "
          | otherwise    = ", "
    envName k = "HTTP_" ++ map conv (BS.unpack $ foldedCase k)
    conv '-' = '_'
    conv c   = toUpper c

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
>>> pathinfoToCGI "/cgi-bin/" "/User/cgi-bin/" "/cgi-bin" "index.cgi"
("/User/cgi-bin/index.cgi","/cgi-bin/index.cgi","")
>>> pathinfoToCGI "/cgi-bin/" "/User/cgi-bin/" "/cgi-bin//foo" "index.cgi"
("/User/cgi-bin/index.cgi","/cgi-bin/index.cgi","/foo")

-}

pathinfoToCGI :: Path -> Path -> Path -> Path -> (FilePath, String, String)
pathinfoToCGI src dst path index = (prog, scriptName, pathinfo)
  where
    path' = path <\> src
    (prog',pathinfo') = case breakAtSeparator path' of
        ("", pinfo) -> (index, pinfo)
        x           -> x
    prog = pathString (dst </> prog')
    scriptName = pathString (src </> prog')
    pathinfo = pathString pathinfo'
