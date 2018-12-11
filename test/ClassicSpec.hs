{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module ClassicSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Network.HTTP
import Network.Stream
import System.IO
import Test.Hspec

spec :: Spec
spec = do
    describe "cgiApp" $ do
        it "accepts POST" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/echo-env/pathinfo?query=foo"
            bdy <- rspBody <$> sendPOST url "foo bar.\nbaz!\n"
            ans <- readFileAscii "test/data/post"
            bdy `shouldBe` ans
        it "causes 500 if the CGI script does not exist" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/broken"
            sc <- rspCode <$> sendPOST url "foo bar.\nbaz!\n"
            sc `shouldBe` (5,0,0)
    describe "fileApp" $ do
        it "returns index.html for /" $ do
            let url = "http://127.0.0.1:2345/"
            bdy <- rspBody <$> sendGET url
            ans <- readFileAscii "test/html/index.html"
            bdy `shouldBe` ans
        it "works with files that lack a file extension" $ do
            let url = "http://127.0.0.1:2345/no_extension"
            bdy <- rspBody <$> sendGET url
            ans <- readFileAscii "test/html/no_extension"
            bdy `shouldBe` ans
        it "returns 400 if not exist" $ do
            let url = "http://127.0.0.1:2345/dummy"
            sc <- rspCode <$> sendGET url
            sc `shouldBe` (4,0,4)
        it "returns Japanese HTML if language is specified" $ do
            let url = "http://127.0.0.1:2345/ja/"
            bdy <- rspBody <$> sendGETwH url [Header HdrAcceptLanguage "ja, en;q=0.7"]
            ans <- readFileAscii "test/html/ja/index.html.ja"
            bdy `shouldBe` ans
        it "returns English HTML even if language is specified" $ do
            let url = "http://127.0.0.1:2345/"
            bdy <- rspBody <$> sendGETwH url [Header HdrAcceptLanguage "ja, en;q=0.7"]
            ans <- readFileAscii "test/html/index.html"
            bdy `shouldBe` ans
        it "returns 304 if not changed" $ do
            let url = "http://127.0.0.1:2345/"
            hdr <- rspHeaders <$> sendGET url
            let Just lm = lookupHeader HdrLastModified hdr
            sc <- rspCode <$> sendGETwH url [Header HdrIfModifiedSince lm]
            sc `shouldBe` (3,0,4)
        it "can handle partial request" $ do
            let url = "http://127.0.0.1:2345/"
                ans = "html>\n<html"
            bdy <- rspBody <$> sendGETwH url [Header HdrRange "bytes=10-20"]
            bdy `shouldBe` ans
        it "can handle HEAD" $ do
            let url = "http://127.0.0.1:2345/"
            sc <- rspCode <$> sendHEAD url
            sc `shouldBe` (2,0,0)
        it "returns 404 for HEAD if not exist" $ do
            let url = "http://127.0.0.1:2345/dummy"
            sc <- rspCode <$> sendHEAD url
            sc `shouldBe` (4,0,4)
        it "can handle HEAD even if language is specified" $ do
            let url = "http://127.0.0.1:2345/ja/"
            sc <- rspCode <$> sendHEADwH url [Header HdrAcceptLanguage "ja, en;q=0.7"]
            sc `shouldBe` (2,0,0)
        it "returns 304 for HEAD if not modified" $ do
            let url = "http://127.0.0.1:2345/"
            hdr <- rspHeaders <$> sendHEAD url
            let Just lm = lookupHeader HdrLastModified hdr
            sc <- rspCode <$> sendHEADwH url [Header HdrIfModifiedSince lm]
            sc `shouldBe` (3,0,4)
        it "redirects to dir/ if trailing slash is missing" $ do
            let url = "http://127.0.0.1:2345/redirect"
            rsp <- sendGET url
            let sc = rspCode rsp
                hdr = rspHeaders rsp
                Just lm = lookupHeader HdrLocation hdr
            sc `shouldBe` (3,0,1)
            lm `shouldBe` "//127.0.0.1:2345/redirect/"

----------------------------------------------------------------

sendGET :: String -> IO (Response String)
sendGET url = sendGETwH url []

sendGETwH :: String -> [Header] -> IO (Response String)
sendGETwH url hdr = unResult $ simpleHTTP $ (getRequest url) { rqHeaders = hdr }

sendHEAD :: String -> IO (Response String)
sendHEAD url = sendHEADwH url []

sendHEADwH :: String -> [Header] -> IO (Response String)
sendHEADwH url hdr = unResult $ simpleHTTP $ (headRequest url) { rqHeaders = hdr }

sendPOST :: String -> String -> IO (Response String)
sendPOST url body = unResult $ simpleHTTP $ postRequestWithBody url "Text/Plain" body

unResult :: IO (Result (Response String)) -> IO (Response String)
unResult action = do
    res <- action
    case res of
        Right rsp -> return rsp
        Left  _   -> error "Connection error"

readFileAscii :: FilePath -> IO String
readFileAscii name = do
   h <- openFile name ReadMode
   hSetEncoding h latin1
   hGetContents h
