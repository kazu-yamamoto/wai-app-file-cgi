{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Status where

import Network.Wai

statusNotModified :: Status
statusNotModified = Status 304 "Not Modified"

statusPreconditionFailed :: Status
statusPreconditionFailed = Status 412 "Precondition Failed"

statusRequestedRangeNotSatisfiable :: Status
statusRequestedRangeNotSatisfiable = Status 416 "Requested Range Not Satisfiable"

statusNotImplemented :: Status
statusNotImplemented = Status 501 "Not Implemented"
