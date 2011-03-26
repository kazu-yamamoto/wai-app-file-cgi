{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.CGI (
    cgiApp
  ) where

import Blaze.ByteString.Builder.ByteString
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Enumerator (Iteratee,Enumeratee,run_,($$),joinI)
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import Network.Wai
import Network.Wai.Application.Classic.EnumLine as ENL
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Types
import Network.Wai.Application.Classic.Utils
import System.FilePath
import System.IO
import System.Process

----------------------------------------------------------------

type ENVVARS = [(String,String)]

gatewayInterface :: String
gatewayInterface = "CGI/1.1"

----------------------------------------------------------------

{-|
  Handle GET and POST for CGI.

The program to link this library must ignore SIGCHLD as follow:

>   installHandler sigCHLD Ignore Nothing
-}
cgiApp :: AppSpec -> CgiRoute -> Application
cgiApp spec cgii req = case method of
    "GET"  -> cgiApp' False spec cgii req
    "POST" -> cgiApp' True  spec cgii req
    _      -> return $ responseLBS statusNotAllowed textPlain "Method Not Allowed"
  where
    method = requestMethod req

cgiApp' :: Bool -> AppSpec -> CgiRoute -> Application
cgiApp' body spec cgii req = do
    naddr <- liftIO . getPeerAddr . remoteHost $ req
    (Just whdl,Just rhdl,_,_) <- liftIO . createProcess . proSpec $ naddr
    liftIO $ do
        hSetEncoding rhdl latin1
        hSetEncoding whdl latin1
    when body $ EL.consume >>= liftIO . mapM_ (BS.hPutStr whdl)
    liftIO . hClose $ whdl
    (return . ResponseEnumerator) (\build ->
        run_ $ EB.enumHandle 4096 rhdl $$ do
            m <- (>>= check) <$> parseHeader
            let (st, hdr, emp) = case m of
                    Nothing    -> (status500,[],True)
                    Just (s,h) -> (s,h,False)
                hdr' = addHeader hdr
            liftIO $ logger spec req st
            if emp
               then emptyBody =$ response build st hdr'
               else              response build st hdr')
  where
    proSpec naddr = CreateProcess {
        cmdspec = RawCommand prog []
      , cwd = Nothing
      , env = Just (makeEnv req naddr scriptName pathinfo (softwareName spec))
      , std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
      }
    (prog, scriptName, pathinfo) = pathinfoToCGI (cgiSrc cgii)
                                                 (cgiDst cgii)
                                                 (pathInfo req)
    toBuilder = EL.map fromByteString
    emptyBody = EB.isolate 0
    response build status hs = toBuilder =$ build status hs
    check hs = lookupField fkContentType hs >> case lookupField "status" hs of
        Nothing -> Just (status200, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        hs' = filter (\(k,_) -> ciLowerCase k /= "status") hs
    toStatus s = BS.readInt s >>= \x -> Just (Status (fst x) s)
    addHeader hdr = ("Server", softwareName spec) : hdr

----------------------------------------------------------------

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
      , ("SERVER_PROTOCOL",   "HTTP/" ++ (BS.unpack . httpVersion $ req))
      , ("SERVER_SOFTWARE",   BS.unpack sname)
      , ("PATH_INFO",         pathinfo)
      , ("QUERY_STRING",      query req)
      ]
    headers = requestHeaders req
    addLength = addEnv "CONTENT_LENGTH" $ lookupField fkContentLength headers
    addType   = addEnv "CONTENT_TYPE" $ lookupField fkContentType headers
    addCookie = addEnv "HTTP_COOKIE" $ lookupField fkCookie headers
    query = BS.unpack . safeTail . queryString
      where
        safeTail "" = ""
        safeTail bs = BS.tail bs

addEnv :: String -> Maybe ByteString -> ENVVARS -> ENVVARS
addEnv _   Nothing    envs = envs
addEnv key (Just val) envs = (key,BS.unpack val) : envs

----------------------------------------------------------------

parseHeader :: Iteratee ByteString IO (Maybe RequestHeaders)
parseHeader = takeHeader >>= maybe (return Nothing)
                                   (return . Just . map parseField)
  where
    parseField bs = (CIByteString key skey, val)
      where
        (key,val) = case BS.break (==':') bs of
            kv@(_,"") -> kv
            (k,v) -> let v' = BS.dropWhile (==' ') $ BS.tail v in (k,v')
        skey = BS.map toLower key

takeHeader :: Iteratee ByteString IO (Maybe [ByteString])
takeHeader = ENL.head >>= maybe (return Nothing) $. \l ->
    if l == ""
       then return (Just [])
       else takeHeader >>= maybe (return Nothing) (return . Just . (l:))

pathinfoToCGI :: ByteString -> FilePath -> ByteString -> (FilePath, String, String)
pathinfoToCGI src dst path = (prog, scriptName, pathinfo)
  where
    src' = BS.unpack src
    path' = drop (BS.length src) $ BS.unpack path
    (prog',pathinfo) = break (== '/') path'
    prog = dst </> prog'
    scriptName = src' </> prog'

----------------------------------------------------------------

infixr 0 =$

(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
ee =$ ie = joinI $ ee $$ ie

----------------------------------------------------------------

infixr 6 $.

($.) :: (a -> b) -> a -> b
($.) = ($)
