{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Types where

import Data.ByteString (ByteString)
import Network.Wai

data AppSpec = AppSpec {
    softwareName :: String
  , indexFile :: FilePath
  , isHTML :: FilePath -> Bool
  }

statusNotModified :: Status
statusNotModified = Status 304 "Not Modified"

statusPreconditionFailed :: Status
statusPreconditionFailed = Status 412 "Precondition Failed"

statusRequestedRangeNotSatisfiable :: Status
statusRequestedRangeNotSatisfiable = Status 416 "Requested Range Not Satisfiable"

type FieldKey = ByteString

fkAcceptLanguage :: ByteString
fkAcceptLanguage = "accept-language"

fkRange :: FieldKey
fkRange = "range"

fkIfRange :: FieldKey
fkIfRange = "if-range"

fkLastModified :: FieldKey
fkLastModified = "last-modified"

fkIfModifiedSince :: FieldKey
fkIfModifiedSince = "if-modified-since"

fkIfUnmodifiedSince :: FieldKey
fkIfUnmodifiedSince = "if-unmodified-since"

fkContentLength :: FieldKey
fkContentLength = "content-length"

fkContentType :: FieldKey
fkContentType = "content-type"

fkCookie :: FieldKey
fkCookie = "cookie"

