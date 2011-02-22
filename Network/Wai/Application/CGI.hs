{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.CGI (cgiApp, CgiInfo(..)) where

import Blaze.ByteString.Builder.ByteString
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Enumerator (($$))
import Data.Enumerator (Iteratee,run_)
import qualified Data.Enumerator as E (map)
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import Data.List (isPrefixOf)
import Data.Maybe
import Network.Socket (getNameInfo, SockAddr, NameInfoFlag(..))
import Network.Wai
import Network.Wai.Application.Utils
import System.FilePath
import System.IO
import System.Process

import Network.Wai.Application.EnumLine as ENL

{-
let cgi = pathinfoToCGI src dst pathi
                  in cgiApp (unpack svr) cgi req
-}

----------------------------------------------------------------

type ENVVARS = [(String,String)]
type NumericAddress = String

data CgiInfo = CgiInfo {
    cgiSrc :: ByteString
  , cgiDst :: FilePath
  , softwareName :: String
  }

gatewayInterface :: String
gatewayInterface = "CGI/1.1"

{-
data CGI = CGI {
    -- | A porgram path to be executed.
    progPath    :: FilePath
    -- | A script name.
  , scriptName  :: String
    -- | A path information.
  , pathinfo    :: String
  } deriving (Eq,Show)
-}

----------------------------------------------------------------

cgiApp :: CgiInfo -> Application
cgiApp cgii req = do
    naddr <- liftIO . getPeerAddr . remoteHost $ req
    (Just whdl,Just rhdl,_,_) <- liftIO . createProcess . proSpec $ naddr
--    liftIO $ run_ EB.consume >>= BL.hPut whdl
    liftIO $ run_ EL.consume >>= mapM_ (BS.hPutStr whdl)
    liftIO . hClose $ whdl
    return . ResponseEnumerator $ \build -> run_ $ EB.enumHandle 4096 rhdl $$
        ((>>= check) <$> parseHeader) >>= maybe (responseNG build)
                                                (responseOK build)
  where
    proSpec naddr = CreateProcess {
        cmdspec = RawCommand prog []
      , cwd = Nothing
      , env = Just (makeEnv req naddr scriptName pathinfo (softwareName cgii))
      , std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
      }
    (prog, scriptName, pathinfo) = pathinfoToCGI (cgiSrc cgii)
                                                 (cgiDst cgii)
                                                 (pathInfo req)
    withBuilder = E.map fromByteString
    emptyBody = EB.isolate 0
    responseOK build (status,hs)  = withBuilder =$ build status hs
    responseNG build = emptyBody =$ withBuilder =$ build status500 []
    check hs = lookupField "content-type" hs >> case lookupField "status" hs of
        Nothing -> Just (status200, hs)
        Just l  -> toStatus l >>= \s -> Just (s,hs')
      where
        hs' = filter (\(k,_) -> ciLowerCase k /= "status") hs
    toStatus s = BS.readInt s >>= \x -> Just (Status (fst x) s)

----------------------------------------------------------------

makeEnv :: Request -> NumericAddress -> String -> String -> String -> ENVVARS
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
      , ("SERVER_SOFTWARE",   sname)
      , ("PATH_INFO",         pathinfo)
      , ("QUERY_STRING",      BS.unpack . queryString $ req)
      ]
    headers = requestHeaders req
    addLength = addEnv "CONTENT_LENGTH" $ lookupField "content-length" headers
    addType   = addEnv "CONTENT_TYPE" $ lookupField "content-type" headers
    addCookie = addEnv "HTTP_COOKIE" $ lookupField "cookie" headers

addEnv :: String -> Maybe ByteString -> ENVVARS -> ENVVARS
addEnv _   Nothing    envs = envs
addEnv key (Just val) envs = (key,BS.unpack val) : envs

----------------------------------------------------------------

lookupField :: ByteString -> RequestHeaders -> Maybe ByteString
lookupField x (((CIByteString _ l), val):kvs)
  | x == l       = Just val
  | otherwise    = lookupField x kvs
lookupField _ [] = Nothing

getPeerAddr :: SockAddr -> IO NumericAddress
getPeerAddr sa = strip . fromJust . snd <$> getInfo sa
  where
    getInfo = getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True
    strip x
      | "::ffff:" `isPrefixOf` x = drop 7 x
      | otherwise                = x

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
