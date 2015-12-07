module Network.Wai.Application.Classic.FileInfo where

import Network.Wai
import Network.Wai.Application.Classic.Path
import Network.Wai.Application.Classic.Types

----------------------------------------------------------------

pathinfoToFilePath :: Request -> FileRoute -> Path
pathinfoToFilePath req filei = path'
  where
    path = fromByteString $ rawPathInfo req
    src = fileSrc filei
    dst = fileDst filei
    path' = dst </> (path <\> src)

addIndex :: FileAppSpec -> Path -> Path
addIndex spec path
  | hasTrailingPathSeparator path = path </> indexFile spec
  | otherwise                     = path

redirectPath :: FileAppSpec -> Path -> Maybe Path
redirectPath spec path
  | hasTrailingPathSeparator path = Nothing
  | otherwise                     = Just (path </> indexFile spec)
