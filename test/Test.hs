{-# LANGUAGE OverloadedStrings #-}

-- % mighty mighty.conf mighty.route
-- % runghc -i.. Test.hs

module Test where

import Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Enumerator
import Network.Wai.Application.Date
import Network.Wai.Application.Lang
import Network.Wai.Application.Range
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

tests :: [Test]
tests = [
    testGroup "default" [
         testCase "lang" test_lang
       , testCase "date" test_date
       , testCase "range" test_range
       ]
  , testGroup "mighty" [
         testCase "post" test_post
       ]
  ]

----------------------------------------------------------------

test_lang :: Assertion
test_lang = do
    let res = parseLang "en-gb;q=0.8, en;q=0.7, da"
    res @?= ans
  where
    ans = ["da","en-gb","en"]

----------------------------------------------------------------

test_date :: Assertion
test_date = do
    let Just x = parseDate date
        res = utcToDate x
    res @?= date
  where
    date = "Tue, 15 Nov 1994 08:12:31 GMT"

----------------------------------------------------------------

test_range :: Assertion
test_range = do
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

test_post :: Assertion
test_post = do
    rsp <- sendHttp url "foo bar.\nbaz!\n"
    ans <- L.readFile "data/post"
    rsp @?= ans
 where
    url = "http://localhost:8080/cgi-bin/echo-env/pathinfo?query"

sendHttp :: String -> L.ByteString -> IO L.ByteString
sendHttp url body = do
    req' <- parseUrl url
    let req = req' {
            method = "POST"
          , requestBody = body
          }
    Response sc _ b <- httpLbsRedirect req
    if 200 <= sc && sc < 300
        then return b
        else error "sendHttp"

----------------------------------------------------------------

main :: Assertion
main = defaultMain tests
