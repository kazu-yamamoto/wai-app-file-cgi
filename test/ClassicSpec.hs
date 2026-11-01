{-# LANGUAGE OverloadedStrings #-}

module ClassicSpec where

import Control.Exception (bracket_)
import Data.List (isPrefixOf)
import qualified Network.HTTP.Client as H
import Network.HTTP
import Network.Stream
import System.Directory (removeFile)
import System.IO
import Test.Hspec

spec :: Spec
spec = do
    describe "cgiApp" $ do
        it "returns index.cgi with empty PATH_INFO for /cgi-bin/" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/"
            bdy <- rspBody <$> sendGET url
            bdy `shouldBe` "index.cgi\n"
        it "decodes PATH_INFO" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/index.cgi/a%20b"
            bdy <- rspBody <$> sendGET url
            bdy `shouldBe` "PATH_INFO: /a b\n"
        it "accepts POST" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/echo-env/pathinfo?query=foo"
            bdy <- rspBody <$> sendPOST url "foo bar.\nbaz!\n"
            ans <- readFileAscii "test/data/post"
            bdy `shouldBe` ans
        it "accepts a large POST echoed back by the CGI" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/cat"
                body = replicate 1000000 'x'
            bdy <- rspBody <$> sendPOST url body
            bdy `shouldBe` body
        it "relays the status of the CGI" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/not-found"
            rsp <- sendGET url
            rspCode rsp `shouldBe` (4, 0, 4)
            words (rspReason rsp) `shouldBe` ["Not", "Found"]
        it "redirects if the CGI returns Location only" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/redirect"
            rsp <- sendGET url
            rspCode rsp `shouldBe` (3, 0, 2)
            lookupHeader HdrLocation (rspHeaders rsp)
                `shouldBe` Just "http://example.com/"
        it "passes request headers as HTTP_* except Proxy and ones with _" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/http-env"
                hdr =
                    [ Header (HdrCustom "X-Foo") "foo"
                    , Header (HdrCustom "X_Foo") "evil"
                    , Header (HdrCustom "Proxy") "http://evil.example/"
                    ]
            bdy <- rspBody <$> sendGETwH url hdr
            lines bdy `shouldBe` ["HTTP_X_FOO: foo", "HTTP_PROXY: ", "REMOTE_PORT: yes"]
        it "passes non-ASCII header values as is" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/http-env"
            bdy <- rspBody <$> sendGETwH url [Header (HdrCustom "X-Foo") "\xe6\x97\xa5"]
            take 1 (lines bdy) `shouldBe` ["HTTP_X_FOO: \xe6\x97\xa5"]
        it "accepts HEAD" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/"
            rsp <- sendHEAD url
            rspCode rsp `shouldBe` (2, 0, 0)
            rspBody rsp `shouldBe` ""
        it "causes 500 if the CGI script does not behave correctly" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/broken"
            sc <- rspCode <$> sendPOST url "foo bar.\nbaz!\n"
            sc `shouldBe` (5, 0, 0)
        it "returns 404 if the CGI does not exist" $ do
            let url = "http://127.0.0.1:2345/cgi-bin/nonexistent"
            sc <- rspCode <$> sendGET url
            sc `shouldBe` (4, 0, 4)
        it "returns 400 if the path contains .." $ do
            let url = "http://127.0.0.1:2345/cgi-bin/../cgi-bin/index.cgi"
            sc <- rspCode <$> sendGET url
            sc `shouldBe` (4, 0, 0)
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
        it "serves files with non-ASCII names" $ do
            -- The path is sent in raw UTF-8 as decoded by mighttpd2.
            let file = "test/html/\26085\26412.txt"
            mgr <- H.newManager H.defaultManagerSettings
            hreq <- H.parseRequest "http://127.0.0.1:2345/"
            let hreq' = hreq{H.path = "/\xe6\x97\xa5\xe6\x9c\xac.txt"}
            bdy <- bracket_ (writeFile file "hello\n") (removeFile file) $
                H.responseBody <$> H.httpLbs hreq' mgr
            bdy `shouldBe` "hello\n"
        it "returns 404 if not exist" $ do
            let url = "http://127.0.0.1:2345/dummy"
            sc <- rspCode <$> sendGET url
            sc `shouldBe` (4, 0, 4)
        it "returns 400 if the path contains .." $ do
            let url = "http://127.0.0.1:2345/../../../../../../etc/hosts"
            sc <- rspCode <$> sendGET url
            sc `shouldBe` (4, 0, 0)
        it "returns 400 if the path contains encoded .." $ do
            let url = "http://127.0.0.1:2345/%2e%2e/%2e%2e/%2e%2e/%2e%2e/etc/hosts"
            sc <- rspCode <$> sendGET url
            sc `shouldBe` (4, 0, 0)
        it "decodes percent-encoded paths" $ do
            let url = "http://127.0.0.1:2345/no%5Fextension"
            bdy <- rspBody <$> sendGET url
            ans <- readFileAscii "test/html/no_extension"
            bdy `shouldBe` ans
        it "returns Japanese HTML if language is specified" $ do
            let url = "http://127.0.0.1:2345/ja/"
            bdy <- rspBody <$> sendGETwH url [Header HdrAcceptLanguage "ja, en;q=0.7"]
            ans <- readFileAscii "test/html/ja/index.html.ja"
            bdy `shouldBe` ans
        it "returns Vary: Accept-Language for HTML" $ do
            let url = "http://127.0.0.1:2345/ja/"
            rsp <- sendGETwH url [Header HdrAcceptLanguage "ja"]
            lookupHeader HdrVary (rspHeaders rsp) `shouldBe` Just "Accept-Language"
        it "returns English HTML even if language is specified" $ do
            let url = "http://127.0.0.1:2345/"
            bdy <- rspBody <$> sendGETwH url [Header HdrAcceptLanguage "ja, en;q=0.7"]
            ans <- readFileAscii "test/html/index.html"
            bdy `shouldBe` ans
        it "returns 304 if not changed" $ do
            let url = "http://127.0.0.1:2345/"
            hdr <- rspHeaders <$> sendGET url
            lm <- maybe (fail "no Last-Modified") return $ lookupHeader HdrLastModified hdr
            sc <- rspCode <$> sendGETwH url [Header HdrIfModifiedSince lm]
            sc `shouldBe` (3, 0, 4)
        it "can handle partial request" $ do
            let url = "http://127.0.0.1:2345/"
                ans = "html>\n<html"
            bdy <- rspBody <$> sendGETwH url [Header HdrRange "bytes=10-20"]
            bdy `shouldBe` ans
        it "can handle HEAD" $ do
            let url = "http://127.0.0.1:2345/"
            sc <- rspCode <$> sendHEAD url
            sc `shouldBe` (2, 0, 0)
        it "returns 405 with Allow for other methods" $ do
            let url = "http://127.0.0.1:2345/"
            rsp <- unResult $ simpleHTTP $ (getRequest url){rqMethod = OPTIONS}
            rspCode rsp `shouldBe` (4, 0, 5)
            lookupHeader HdrAllow (rspHeaders rsp) `shouldBe` Just "GET, HEAD"
        it "returns 404 for HEAD if not exist" $ do
            let url = "http://127.0.0.1:2345/dummy"
            sc <- rspCode <$> sendHEAD url
            sc `shouldBe` (4, 0, 4)
        it "can handle HEAD even if language is specified" $ do
            let url = "http://127.0.0.1:2345/ja/"
            sc <- rspCode <$> sendHEADwH url [Header HdrAcceptLanguage "ja, en;q=0.7"]
            sc `shouldBe` (2, 0, 0)
        it "returns 304 for HEAD if not modified" $ do
            let url = "http://127.0.0.1:2345/"
            hdr <- rspHeaders <$> sendHEAD url
            lm <- maybe (fail "no Last-Modified") return $ lookupHeader HdrLastModified hdr
            sc <- rspCode <$> sendHEADwH url [Header HdrIfModifiedSince lm]
            sc `shouldBe` (3, 0, 4)
        it "redirects to dir/ if trailing slash is missing" $ do
            let url = "http://127.0.0.1:2345/redirect"
            rsp <- sendGET url
            rspCode rsp `shouldBe` (3, 0, 1)
            lookupHeader HdrLocation (rspHeaders rsp)
                `shouldBe` Just "//127.0.0.1:2345/redirect/"
        it "keeps the query string when redirecting to dir/" $ do
            let url = "http://127.0.0.1:2345/redirect?foo=bar"
            rsp <- sendGET url
            lookupHeader HdrLocation (rspHeaders rsp)
                `shouldBe` Just "//127.0.0.1:2345/redirect/?foo=bar"
        it "returns index.html for /exact-route.html matching exactRoute" $ do
            let url = "http://127.0.0.1:2345/exact-route.html"
            bdy <- rspBody <$> sendGET url
            ans <- readFileAscii "test/html/index.html"
            bdy `shouldBe` ans
    describe "redirectApp" $ do
        it "redirects keeping the query string" $ do
            let url = "http://127.0.0.1:2345/redirect-app/foo?bar=baz"
            rsp <- sendGET url
            rspCode rsp `shouldBe` (3, 0, 1)
            lookupHeader HdrLocation (rspHeaders rsp)
                `shouldBe` Just "//example.com/new/foo?bar=baz"
    describe "revProxyApp" $ do
        it "returns 502 if the upstream is not available" $ do
            let url = "http://127.0.0.1:2345/revproxy/"
            sc <- rspCode <$> sendGET url
            sc `shouldBe` (5, 0, 2)
        it "removes hop-by-hop headers and appends X-Forwarded-For and Via" $ do
            let url = "http://127.0.0.1:2345/upstream/"
                hdr =
                    [ Header HdrProxyAuthorization "secret"
                    , Header (HdrCustom "X-Forwarded-For") "192.0.2.1"
                    ]
            rsp <- sendGETwH url hdr
            let ls = lines $ rspBody rsp
                rhdr = rspHeaders rsp
            ls `shouldContain` ["x-forwarded-for: 192.0.2.1, 127.0.0.1"]
            ls `shouldContain` ["via: 1.1 127.0.0.1:2345 (ClassicTester)"]
            filter (isPrefixOf "proxy-authorization") ls `shouldBe` []
            lookupHeader (HdrCustom "X-Up") rhdr `shouldBe` Nothing
            lookupHeader (HdrCustom "Keep-Alive") rhdr `shouldBe` Nothing
            lookupHeader HdrVia rhdr `shouldSatisfy` (/= Nothing)

----------------------------------------------------------------

sendGET :: String -> IO (Response String)
sendGET url = sendGETwH url []

sendGETwH :: String -> [Header] -> IO (Response String)
sendGETwH url hdr = unResult $ simpleHTTP $ (getRequest url){rqHeaders = hdr}

sendHEAD :: String -> IO (Response String)
sendHEAD url = sendHEADwH url []

sendHEADwH :: String -> [Header] -> IO (Response String)
sendHEADwH url hdr = unResult $ simpleHTTP $ (headRequest url){rqHeaders = hdr}

sendPOST :: String -> String -> IO (Response String)
sendPOST url body = unResult $ simpleHTTP $ postRequestWithBody url "Text/Plain" body

unResult :: IO (Result (Response String)) -> IO (Response String)
unResult action = do
    res <- action
    case res of
        Right rsp -> return rsp
        Left _ -> error "Connection error"

readFileAscii :: FilePath -> IO String
readFileAscii name = do
    h <- openFile name ReadMode
    hSetEncoding h latin1
    hGetContents h
