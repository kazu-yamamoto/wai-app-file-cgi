{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

-- % mighty mighty.conf mighty.route
-- % runghc -i.. Test.hs

module Test where

import Control.Exception.Lifted
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit
import Network.HTTP.Date
import qualified Network.HTTP.Types as H
import Network.Wai.Application.Classic.Header
import Network.Wai.Application.Classic.Lang
import Network.Wai.Application.Classic.Range
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime
import Test.HUnit
import Prelude hiding (catch)

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

case_lang :: Assertion
case_lang = do
    let res = parseLang "en-gb;q=0.8, en;q=0.7, da"
    res @?= ans
  where
    ans = ["da","en-gb","en"]

----------------------------------------------------------------

case_date :: Assertion
case_date = do
    let Just x = parseHTTPDate date
        res = formatHTTPDate x
    res @?= date
  where
    date = "Tue, 15 Nov 1994 08:12:31 GMT"

----------------------------------------------------------------

case_range :: Assertion
case_range = do
    let res1 = skipAndSize range1 size
        res2 = skipAndSize range2 size
        res3 = skipAndSize range3 size
        res4 = skipAndSize range4 size
    res1 @?= ans1
    res2 @?= ans2
    res3 @?= ans3
    res4 @?= ans4
  where
    size = 10000
    range1 = "bytes=0-399"
    range2 = "bytes=500-799"
    range3 = "bytes=-500"
    range4 = "bytes=9500-"
    ans1 = Just (0,400)
    ans2 = Just (500,300)
    ans3 = Just (9500,500)
    ans4 = Just (9500,500)

----------------------------------------------------------------

case_post :: Assertion
case_post = do
    Response _ _ bdy <- sendPOST url "foo bar.\nbaz!\n"
    ans <- BL.readFile "data/post"
    bdy @?= ans
  where
    url = "http://localhost:8080/cgi-bin/echo-env/pathinfo?query=foo"

case_post2 :: Assertion
case_post2 = do
    Response sc _ _ <- sendPOST url "foo bar.\nbaz!\n"
    sc @?= H.statusServerError
  where
    url = "http://localhost:8080/cgi-bin/broken"

----------------------------------------------------------------

case_get :: Assertion
case_get = do
    rsp <- simpleHttp url
    ans <- BL.readFile "html/index.html"
    rsp @?= ans
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_get2 :: Assertion
case_get2 = do
    req <- parseUrl url
    Response sc _ _ <- safeHttpLbs req
    sc @?= H.statusNotFound
  where
    url = "http://localhost:8080/dummy"

----------------------------------------------------------------

case_get_ja :: Assertion
case_get_ja = do
    Response _ _ bdy <- sendGET url [("Accept-Language", "ja, en;q=0.7")]
    ans <- BL.readFile "html/ja/index.html.ja"
    bdy @?= ans
  where
    url = "http://localhost:8080/ja/"

----------------------------------------------------------------

case_get_modified :: Assertion
case_get_modified = do
    Response _ hdr _ <- sendGET url []
    let Just lm = lookup fkLastModified hdr
    Response sc _ _ <- sendGET url [("If-Modified-Since", lm)]
    sc @?= H.statusNotModified
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_get_partial :: Assertion
case_get_partial = do
    Response _ _ bdy <- sendGET url [("Range", "bytes=10-20")]
    bdy @?= ans
  where
    url = "http://localhost:8080/"
    ans = "html>\n<html"

----------------------------------------------------------------

case_head :: Assertion
case_head = do
    Response sc _ _ <- sendHEAD url []
    sc @?= H.statusOK
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_head2 :: Assertion
case_head2 = do
    Response sc _ _ <- sendHEAD url []
    sc @?= H.statusNotFound
  where
    url = "http://localhost:8080/dummy"

----------------------------------------------------------------

case_head_ja :: Assertion
case_head_ja = do
    Response sc _ _ <- sendHEAD url [("Accept-Language", "ja, en;q=0.7")]
    sc @?= H.statusOK
  where
    url = "http://localhost:8080/ja/"

----------------------------------------------------------------

case_head_modified :: Assertion
case_head_modified = do
    Response _ hdr _ <- sendHEAD url []
    let Just lm = lookup fkLastModified hdr
    Response sc _ _ <- sendHEAD url [("If-Modified-Since", lm)]
    sc @?= H.statusNotModified
  where
    url = "http://localhost:8080/"

----------------------------------------------------------------

case_redirect :: Assertion
case_redirect = do
    rsp <- simpleHttp url
    ans <- BL.readFile "html/redirect/index.html"
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

safeHttpLbs :: Request IO -> IO (Response BL.ByteString)
safeHttpLbs req = withManager (httpLbs req) `catch` httpHandler

----------------------------------------------------------------

httpHandler :: HttpException -> IO (Response BL.ByteString)
httpHandler (StatusCodeException sc hd) = return (Response sc hd BL.empty)
httpHandler _                           = error "handler"
