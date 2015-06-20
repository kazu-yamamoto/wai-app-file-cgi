{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Path (
    Path(..)
  , fromString, fromByteString
  , (+++), (</>), (<\>), (<.>)
  , breakAtSeparator, hasLeadingPathSeparator, hasTrailingPathSeparator
  , isSuffixOf
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Function
import Data.String
import Data.Word

----------------------------------------------------------------

-- | Smart file path.
data Path = Path {
    pathString :: FilePath
  , pathByteString :: ByteString
  }

instance IsString Path where
    fromString path = Path {
        pathString = path
      , pathByteString = B8.pack path
      }

instance Show Path where
    show = show . pathByteString

instance Eq Path where
    (==) = (==) `on` pathByteString

----------------------------------------------------------------

fromByteString :: ByteString -> Path
fromByteString path = Path {
    pathString = B8.unpack path
  , pathByteString = path
  }

-- pathDot :: Word8
-- pathDot = 46

pathDotBS :: ByteString
pathDotBS = "."

pathSep :: Word8
pathSep = 47

pathSepBS :: ByteString
pathSepBS = "/"

{-|
  Checking if the path ends with the path separator.

>>> hasLeadingPathSeparator "/foo/bar"
True
>>> hasLeadingPathSeparator "foo/bar"
False
-}
hasLeadingPathSeparator :: Path -> Bool
hasLeadingPathSeparator path
  | BS.null bs            = False
  | BS.head bs == pathSep = True
  | otherwise             = False
  where
    bs = pathByteString path

{-|
  Checking if the path ends with the path separator.

>>> hasTrailingPathSeparator "/foo/bar/"
True
>>> hasTrailingPathSeparator "/foo/bar"
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
p1 +++ p2 = fromByteString p
  where
    p = pathByteString p1 `BS.append` pathByteString p2

{-|
  Appending with the file separator.

>>> "/foo" </> "bar"
"/foo/bar"
>>> "/foo/" </> "bar"
"/foo/bar"
>>> "/foo" </> "/bar"
"/foo/bar"
>>> "/foo/" </> "/bar"
"/foo/bar"
-}

(</>) :: Path -> Path -> Path
p1 </> p2
  | has1 && not has2 || not has1 && has2 = p1 +++ p2
  | has1      = fromByteString pp1
  | otherwise = fromByteString pp2
  where
    has1 = hasTrailingPathSeparator p1
    has2 = hasLeadingPathSeparator p2
    p1' = pathByteString p1
    p2' = pathByteString p2
    pp1 = p1' `BS.append` BS.tail p2'
    pp2 = BS.concat [p1',pathSepBS,p2']

{-|
  Removing prefix. The prefix of the second argument is removed
  from the first argument.

>>> "foobar" <\> "foo"
"bar"
>>> "foo" <\> "foobar"
""
>>> "foobar" <\> "baz"
"bar"
-}
(<\>) :: Path -> Path -> Path
p1 <\> p2 = fromByteString p
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
    p = BS.drop (BS.length p2') p1'

{-|
  Adding suffix.
-}
(<.>) :: Path -> Path -> Path
p1 <.> p2 = fromByteString p
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
    p = BS.concat [p1',pathDotBS,p2']

{-|
  Breaking at the first path separator.

>>> breakAtSeparator "/foo/bar/baz"
("","/foo/bar/baz")
>>> breakAtSeparator "foo/bar/baz"
("foo","/bar/baz")
>>> breakAtSeparator "foo"
("foo","")
-}
breakAtSeparator :: Path -> (Path,Path)
breakAtSeparator p = (fromByteString r1, fromByteString r2)
  where
    p' = pathByteString p
    (r1,r2) = BS.break (== pathSep) p'

isSuffixOf :: Path -> Path -> Bool
isSuffixOf p1 p2 = p1' `BS.isSuffixOf` p2'
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
