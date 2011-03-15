{-# LANGUAGE OverloadedStrings #-}

-- % mighty mighty.conf mighty.route

module Test where

import Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Enumerator
import Network.Wai.Application.Lang
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

tests :: [Test]
tests = [
    testGroup "default" [
         testCase "lang" test_lang
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
