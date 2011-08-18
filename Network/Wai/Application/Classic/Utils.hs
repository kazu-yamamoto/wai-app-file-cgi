{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Utils (
    hasTrailingPathSeparator, pathSep
  , (+++), (</>)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Data.Word

pathSep :: Word8
pathSep = 47

hasTrailingPathSeparator :: ByteString -> Bool
hasTrailingPathSeparator "" = False
hasTrailingPathSeparator path
  | BS.last path == pathSep = True
  | otherwise               = False

infixr +++

(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append

(</>) :: ByteString -> ByteString -> ByteString
s1 </> s2
  | hasTrailingPathSeparator s1 = s1 +++ s2
  | otherwise                   = s1 +++ (pathSep `BS.cons` s2)
