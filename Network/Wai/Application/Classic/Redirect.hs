{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Redirect (
    redirectApp
  ) where

import Data.ByteString.Char8
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic.Field
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types

redirectApp :: ClassicAppSpec -> RedirectRoute -> Application
redirectApp cspec route req respond = do
    logger cspec req status Nothing
    respond $ responseLBS status hdr ""
  where
    path = fromByteString $ rawPathInfo req
    src = redirectSrc route
    dst = redirectDst route
    rurl = "http://" `append` pathByteString (dst </> (path <\> src))
    hdr = locationHeader rurl
    status = movedPermanently301
