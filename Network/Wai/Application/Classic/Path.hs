{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Network.Wai.Application.Classic.Path (
    Path
  , pathString
  , fromString
  , (</>), (<\>), (<.>)
  , breakAtSeparator, hasLeadingPathSeparator, hasTrailingPathSeparator
  , isSuffixOf
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.String
import Data.Word

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

-- | File path.
type Path = ByteString

pathString :: Path -> String
pathString = B8.unpack
{-# INLINE pathString #-}

----------------------------------------------------------------

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
hasLeadingPathSeparator bs
  | BS.null bs            = False
  | BS.head bs == pathSep = True
  | otherwise             = False
{-# INLINE hasLeadingPathSeparator #-}

{-|
  Checking if the path ends with the path separator.

>>> hasTrailingPathSeparator "/foo/bar/"
True
>>> hasTrailingPathSeparator "/foo/bar"
False
-}
hasTrailingPathSeparator :: Path -> Bool
hasTrailingPathSeparator bs
  | BS.null bs            = False
  | BS.last bs == pathSep = True
  | otherwise             = False
{-# INLINE hasTrailingPathSeparator #-}

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
p1 </> p2 = p
  where
    !has1 = hasTrailingPathSeparator p1
    !has2 = hasLeadingPathSeparator p2
    !p | has1 && not has2 = p1 `BS.append` p2
       | not has1 && has2 = p1 `BS.append` p2
       | has1             = p1 `BS.append` BS.tail p2
       | otherwise        = BS.concat [p1,pathSepBS,p2]
{-# INLINE (</>) #-}

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
p1 <\> p2 = p
  where
    !p = BS.drop (BS.length p2) p1
{-# INLINE (<\>) #-}

{-|
  Adding suffix.
-}
(<.>) :: Path -> Path -> Path
p1 <.> p2 = p
  where
    !p = BS.concat [p1,pathDotBS,p2]
{-# INLINE (<.>) #-}

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
breakAtSeparator p = BS.break (== pathSep) p
{-# INLINE breakAtSeparator #-}

isSuffixOf :: Path -> Path -> Bool
isSuffixOf = BS.isSuffixOf
{-# INLINE isSuffixOf #-}
