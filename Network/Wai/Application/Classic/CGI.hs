{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.CGI (
    cgiApp
  ) where

import Blaze.ByteString.Builder.ByteString
import Control.Applicative
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
    let naddr = getPeerAddr . remoteHost $ req
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
            liftIO $ logger spec req st Nothing -- cannot know body length
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
                                                 (rawPathInfo req)
    toBuilder = EL.map fromByteString
    emptyBody = EB.isolate 0
    response build status hs = toBuilder =$ build status hs
    check hs = lookupField fkContentType hs >> case lookupField "status" hs of
        Nothing -> Just (status200, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        hs' = filter (\(k,_) -> foldedCase k /= "status") hs
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
      , ("SERVER_PROTOCOL",   show . httpVersion $ req)
      , ("SERVER_SOFTWARE",   BS.unpack sname)
      , ("PATH_INFO",         pathinfo)
      , ("QUERY_STRING",      query req)
      ]
    headers = requestHeaders req
    addLength = addEnv "CONTENT_LENGTH" $ lookupField fkContentLength headers
    addType   = addEnv "CONTENT_TYPE" $ lookupField fkContentType headers
    addCookie = addEnv "HTTP_COOKIE" $ lookupField fkCookie headers
    query = BS.unpack . safeTail . rawQueryString
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
