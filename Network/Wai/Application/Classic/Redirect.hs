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
redirectApp _ route req respond =
    respond $ responseLBS status hdr ""
  where
    path = fromByteString $ rawPathInfo req
    src = redirectSrc route
    dst = redirectDst route
    -- Scheme must not be included because of no way to tell
    -- http or https.
    rurl = "//" `append` pathByteString (dst </> (path <\> src))
    hdr = locationHeader rurl
    status = movedPermanently301
