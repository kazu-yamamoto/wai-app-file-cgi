{-# LANGUAGE OverloadedStrings, CPP #-}

module Network.Wai.Application.Classic.CGI (
    cgiApp
  ) where

import Blaze.ByteString.Builder.ByteString
import Control.Applicative
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (unpack)
import qualified Data.ByteString.Char8 as BS (readInt, unpack)
import Data.CaseInsensitive hiding (map)
import Data.Enumerator hiding (map, filter, drop, break)
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.EnumLine as ENL
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import Network.Wai.Logger.Utils
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
cgiApp :: AppSpec -> CgiRoute -> Application
cgiApp spec cgii req = case method of
    "GET"  -> cgiApp' False spec cgii req
    "POST" -> cgiApp' True  spec cgii req
    _      -> return $ responseLBS statusNotAllowed textPlain "Method Not Allowed\r\n"
  where
    method = requestMethod req

cgiApp' :: Bool -> AppSpec -> CgiRoute -> Application
cgiApp' body spec cgii req = do
    (rhdl,whdl,pid) <- liftIO $ execProcess spec cgii req
    let cleanup = do
            hClose whdl
            hClose rhdl
            terminateProcess pid -- SIGTERM
    -- HTTP body can be obtained in this Iteratee level only
    toCGI whdl body `catchError` const (liftIO cleanup)
    liftIO $ hClose whdl
    respEnumerator $ \hdrMaker ->
        -- this is IO
        fromCGI rhdl spec req hdrMaker `finally` do cleanup
  where
    respEnumerator = return . ResponseEnumerator

----------------------------------------------------------------

toCGI :: Handle -> Bool -> Iteratee ByteString IO ()
toCGI whdl body = when body $ EL.consume >>= liftIO . mapM_ (BS.hPutStr whdl)

fromCGI :: Handle -> AppSpec -> Request -> ResponseEnumerator a
fromCGI rhdl spec req hdrMaker = run_ $ EB.enumHandle 4096 rhdl $$ do
    -- consuming the header part of CGI output
    m <- (>>= check) <$> parseHeader
    let (st, hdr, hasBody) = case m of
            Nothing    -> (status500,[],False)
            Just (s,h) -> (s,h,True)
        hdr' = addHeader hdr
    -- logging
    liftIO $ logger spec req st Nothing -- cannot know body length
    -- building HTTP header and optionally HTTP body
    if hasBody
        then {- Body -}   response st hdr'
        else emptyBody $$ response st hdr'
  where
    toBuilder = EL.map fromByteString
    emptyBody = enumEOF
    response status hs = toBuilder =$ hdrMaker status hs
    check hs = lookup fkContentType hs >> case lookup "status" hs of
        Nothing -> Just (status200, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        hs' = filter (\(k,_) -> k /= "status") hs
    toStatus s = BS.readInt s >>= \x -> Just (Status (fst x) s)
    addHeader hdr = ("Server", softwareName spec) : hdr

----------------------------------------------------------------

parseHeader :: Iteratee ByteString IO (Maybe RequestHeaders)
parseHeader = takeHeader >>= maybe (return Nothing)
                                   (return . Just . map parseField)
  where
    parseField bs = (mk key, val)
      where
        (key,val) = case BS.breakByte 58 bs of -- ':'
            kv@(_,"") -> kv
            (k,v) -> let v' = BS.dropWhile (==32) $ BS.tail v in (k,v') -- ' '

takeHeader :: Iteratee ByteString IO (Maybe [ByteString])
takeHeader = ENL.head >>= maybe (return Nothing) $. \l ->
    if l == ""
       then return (Just [])
       else takeHeader >>= maybe (return Nothing) (return . Just . (l:))

----------------------------------------------------------------

execProcess :: AppSpec -> CgiRoute -> Request -> IO (Handle, Handle, ProcessHandle)
execProcess spec cgii req = do
    let naddr = showSockAddr . remoteHost $ req
    (Just whdl,Just rhdl,_,pid) <- createProcess . proSpec $ naddr
    hSetEncoding rhdl latin1
    hSetEncoding whdl latin1
    return (rhdl, whdl, pid)
 where
    proSpec naddr = CreateProcess {
        cmdspec = RawCommand prog []
      , cwd = Nothing
      , env = Just (makeEnv req naddr scriptName pathinfo (softwareName spec))
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
                                                 (rawPathInfo req)

makeEnv :: Request -> NumericAddress -> String -> String -> ByteString -> ENVVARS
makeEnv req naddr scriptName pathinfo sname = addLength . addType . addCookie $ baseEnv
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
    addLength = addEnv "CONTENT_LENGTH" $ lookup fkContentLength headers
    addType   = addEnv "CONTENT_TYPE" $ lookup fkContentType headers
    addCookie = addEnv "HTTP_COOKIE" $ lookup fkCookie headers
    query = BS.unpack . safeTail . rawQueryString
      where
        safeTail "" = ""
        safeTail bs = BS.tail bs

addEnv :: String -> Maybe ByteString -> ENVVARS -> ENVVARS
addEnv _   Nothing    envs = envs
addEnv key (Just val) envs = (key,BS.unpack val) : envs

pathinfoToCGI :: ByteString -> ByteString -> ByteString -> (FilePath, String, String)
pathinfoToCGI src dst path = (prog, scriptName, pathinfo)
  where
    path' = BS.drop (BS.length src) path
    (prog',pathinfo') = BS.breakByte pathSep path'
    prog = BS.unpack (dst </> prog')
    scriptName = BS.unpack (src </> prog')
    pathinfo = BS.unpack pathinfo'

----------------------------------------------------------------

infixr 6 $.

($.) :: (a -> b) -> a -> b
($.) = ($)
