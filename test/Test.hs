{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Control.Exception.Lifted
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as H
import Prelude hiding (catch)
import Test.Framework.Providers.DocTest
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime
import Test.HUnit

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

case_post :: Assertion
case_post = do
    Response _ _ _ bdy <- sendPOST url "foo bar.\nbaz!\n"
    ans <- BL.readFile "test/data/post"
    bdy @?= ans
  where
    url = "http://localhost:8080/cgi-bin/echo-env/pathinfo?query=foo"

case_post2 :: Assertion
case_post2 = do
    Response sc _ _ _ <- sendPOST url "foo bar.\nbaz!\n"
    sc @?= H.internalServerError500
  where
    url = "http://localhost:8080/cgi-bin/broken"

----------------------------------------------------------------

case_get :: Assertion
case_get = do
    rsp <- simpleHttp url
    ans <- BL.readFile "test/html/index.html"
    rsp @?= ans
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_get2 :: Assertion
case_get2 = do
    req <- parseUrl url
    Response sc _ _ _ <- safeHttpLbs req
    sc @?= H.notFound404
  where
    url = "http://localhost:8080/dummy"

----------------------------------------------------------------

case_get_ja :: Assertion
case_get_ja = do
    Response _ _ _ bdy <- sendGET url [("Accept-Language", "ja, en;q=0.7")]
    ans <- BL.readFile "test/html/ja/index.html.ja"
    bdy @?= ans
  where
    url = "http://localhost:8080/ja/"

----------------------------------------------------------------

case_get_modified :: Assertion
case_get_modified = do
    Response _ _ hdr _ <- sendGET url []
    let Just lm = lookup "Last-Modified" hdr
    Response sc _ _ _ <- sendGET url [("If-Modified-Since", lm)]
    sc @?= H.notModified304
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_get_partial :: Assertion
case_get_partial = do
    Response _ _ _ bdy <- sendGET url [("Range", "bytes=10-20")]
    bdy @?= ans
  where
    url = "http://localhost:8080/"
    ans = "html>\n<html"

----------------------------------------------------------------

case_head :: Assertion
case_head = do
    Response sc _ _ _ <- sendHEAD url []
    sc @?= H.ok200
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_head2 :: Assertion
case_head2 = do
    Response sc _ _ _ <- sendHEAD url []
    sc @?= H.notFound404
  where
    url = "http://localhost:8080/dummy"

----------------------------------------------------------------

case_head_ja :: Assertion
case_head_ja = do
    Response sc _ _ _ <- sendHEAD url [("Accept-Language", "ja, en;q=0.7")]
    sc @?= H.ok200
  where
    url = "http://localhost:8080/ja/"

----------------------------------------------------------------

case_head_modified :: Assertion
case_head_modified = do
    Response _ _ hdr _ <- sendHEAD url []
    let Just lm = lookup "Last-Modified" hdr
    Response sc _ _ _ <- sendHEAD url [("If-Modified-Since", lm)]
    sc @?= H.notModified304
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_redirect :: Assertion
case_redirect = do
    rsp <- simpleHttp url
    ans <- BL.readFile "test/html/redirect/index.html"
    rsp @?= ans
  where
    url = "http://localhost:8080/redirect"

----------------------------------------------------------------
----------------------------------------------------------------

sendGET ::String -> H.RequestHeaders -> IO (Response BL.ByteString)
sendGET url hdr = do
    req' <- parseUrl url
    let req = req' { requestHeaders = hdr }
    safeHttpLbs req

----------------------------------------------------------------

sendHEAD :: String -> H.RequestHeaders -> IO (Response BL.ByteString)
sendHEAD url hdr = do
    req' <- parseUrl url
    let req = req' {
            requestHeaders = hdr
          , method = "HEAD"
          }
    safeHttpLbs req

----------------------------------------------------------------

sendPOST :: String -> BL.ByteString -> IO (Response BL.ByteString)
sendPOST url body = do
    req' <- parseUrl url
    let req = req' {
            method = "POST"
          , requestBody = RequestBodyLBS body
          }
    safeHttpLbs req

----------------------------------------------------------------

safeHttpLbs :: Request (ResourceT IO) -> IO (Response BL.ByteString)
safeHttpLbs req = withManager (httpLbs req) `catch` httpHandler

----------------------------------------------------------------

httpHandler :: HttpException -> IO (Response BL.ByteString)
httpHandler (StatusCodeException sc hd) = return (Response sc H.http11 hd BL.empty)
httpHandler _                           = error "handler"
