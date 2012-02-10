{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Path (
    Path(..)
  , fromString, fromByteString
  , (+++), (</>), (<\>), (<.>)
  , breakAtSeparator, hasTrailingPathSeparator
  , isSuffixOf
  ) where

import qualified Blaze.ByteString.Builder as BB
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (null, last, append, drop, length, breakByte, isSuffixOf)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Monoid
import Data.String
import Data.Word
import Data.Function

----------------------------------------------------------------

-- | Smart file path.
data Path = Path {
    pathString :: FilePath
  , pathByteString :: ByteString
  }

instance IsString Path where
    fromString path = Path {
        pathString = path
      , pathByteString = BS.pack path
      }

instance Show Path where
    show = show . pathByteString

instance Eq Path where
    (==) = (==) `on` pathByteString

----------------------------------------------------------------

fromByteString :: ByteString -> Path
fromByteString path = Path {
    pathString = BS.unpack path
  , pathByteString = path
  }

pathDot :: Word8
pathDot = 46

pathSep :: Word8
pathSep = 47

{-|
  Checking if the path ends with the path separator.

>>> hasTrailingPathSeparator "/foo/bar/"
True
>>>  hasTrailingPathSeparator "/foo/bar"
False
-}
hasTrailingPathSeparator :: Path -> Bool
hasTrailingPathSeparator path
  | BS.null bs            = False
  | BS.last bs == pathSep = True
  | otherwise             = False
  where
    bs = pathByteString path

infixr +++

{-|
  Appending.
-}

(+++) :: Path -> Path -> Path
p1 +++ p2 = Path {
    pathString = BS.unpack p
  , pathByteString = p
  }
  where
    p = pathByteString p1 `BS.append` pathByteString p2

{-|
  Appending with the file separator.
-}

(</>) :: Path -> Path -> Path
p1 </> p2
  | hasTrailingPathSeparator p1 = p1 +++ p2
  | otherwise                   = Path {
      pathString = BS.unpack p
    , pathByteString = p
    }
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
    p = BB.toByteString (BB.fromByteString p1'
                       `mappend` BB.fromWord8 pathSep
                       `mappend` BB.fromByteString p2')

{-|
  Removing prefix. The prefix of the second argument is removed
  from the first argument.
-}
(<\>) :: Path -> Path -> Path
p1 <\> p2 = Path {
    pathString = BS.unpack p
  , pathByteString = p
  }
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
    p = BS.drop (BS.length p2') p1'

{-|
  Adding suffix.
-}
(<.>) :: Path -> Path -> Path
p1 <.> p2 = Path {
    pathString = BS.unpack p
  , pathByteString = p
  }
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
    p = BB.toByteString (BB.fromByteString p1'
                       `mappend` BB.fromWord8 pathDot
                       `mappend` BB.fromByteString p2')

{-|
  Breaking at the first path separator.
-}
breakAtSeparator :: Path -> (Path,Path)
breakAtSeparator p = (fromByteString r1, fromByteString r2)
  where
    p' = pathByteString p
    (r1,r2) = BS.breakByte pathSep p'

isSuffixOf :: Path -> Path -> Bool
isSuffixOf p1 p2 = p1' `BS.isSuffixOf` p2'
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
