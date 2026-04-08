module Network.Wai.Application.Classic.FileInfo where

import Network.Wai
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types

----------------------------------------------------------------

{- |
>>> :set -XOverloadedStrings
>>> import Network.Wai (defaultRequest)
>>> import Network.Wai.Internal (Request (..))
>>> mkReq p = defaultRequest { rawPathInfo = p }
>>> pathinfoToFilePath (mkReq "/") (FileRoute "/" "/srv/http")
"/srv/http/"
>>> pathinfoToFilePath (mkReq "/") (FileRoute "/" "/srv/http/")
"/srv/http/"
>>> pathinfoToFilePath (mkReq "/hello") (FileRoute "/" "/srv/http/")
"/srv/http/hello"
>>> pathinfoToFilePath (mkReq "/hello/") (FileRoute "/" "/srv/http/")
"/srv/http/hello/"
>>> pathinfoToFilePath (mkReq "/about/wai.html") (FileRoute "/" "/srv/http/")
"/srv/http/about/wai.html"
>>> pathinfoToFilePath (mkReq "/hello/") (FileRoute "/sub/dir/" "/var/root/")
"/var/root/"
>>> pathinfoToFilePath (mkReq "/sub/dir/") (FileRoute "/sub/dir/" "/var/root/")
"/var/root/"
>>> pathinfoToFilePath (mkReq "/sub/dir/test.html") (FileRoute "/sub/dir/" "/var/root/")
"/var/root/test.html"
>>> pathinfoToFilePath (mkReq "/exact/match") (FileRoute "/exact/match" "/tmp/dst")
"/tmp/dst/"
>>> pathinfoToFilePath (mkReq "/exact/match/") (FileRoute "/exact/match" "/tmp/dst")
"/tmp/dst/"
>>> pathinfoToFilePath (mkReq "/exact/match/more.html") (FileRoute "/exact/match" "/tmp/dst")
"/tmp/dst/more.html"
-}
pathinfoToFilePath :: Request -> FileRoute -> Path
pathinfoToFilePath req filei = path'
  where
    path = rawPathInfo req
    src = fileSrc filei
    dst = fileDst filei
    path' = dst </> (path <\> src) -- fixme

addIndex :: FileAppSpec -> Path -> Path
addIndex spec path
  | hasTrailingPathSeparator path = path </> indexFile spec
  | otherwise                     = path

redirectPath :: FileAppSpec -> Path -> Maybe Path
redirectPath spec path
  | hasTrailingPathSeparator path = Nothing
  | otherwise                     = Just (path </> indexFile spec)
