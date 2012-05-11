{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Redirect (
    redirectApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types

redirectApp :: ClassicAppSpec -> RedirectRoute -> Application
redirectApp cspec route req = do
    liftIO $ logger cspec req status Nothing
    return $ responseLBS status hdr ""
  where
    path = fromByteString $ rawPathInfo req
    src = redirectSrc route
    dst = redirectDst route
    rurl = "http://" `append` pathByteString (dst </> (path <\> src))
    hdr = addServer cspec $ locationHeader rurl
    status = movedPermanently301
