{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Utils (
    Path(..)
  , fromString, fromByteString
  , (+++), (</>), (<\>), (<.>)
  , breakSep, concatByteString, hasTrailingPathSeparator
  ) where

import qualified Blaze.ByteString.Builder as BB
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (null, last, append, drop, length, breakByte)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Monoid
import Data.String
import Data.Word

----------------------------------------------------------------

data Path = Path {
    pathString :: FilePath
  , pathByteString :: ByteString
  }

instance IsString Path where
    fromString path = Path {
        pathString = path
      , pathByteString = BS.pack path
      }

fromByteString :: ByteString -> Path
fromByteString path = Path {
    pathString = BS.unpack path
  , pathByteString = path
  }

pathDot :: Word8
pathDot = 46

pathSep :: Word8
pathSep = 47

hasTrailingPathSeparator :: Path -> Bool
hasTrailingPathSeparator path
  | BS.null bs            = False
  | BS.last bs == pathSep = True
  | otherwise             = False
  where
    bs = pathByteString path

infixr +++

(+++) :: Path -> Path -> Path
p1 +++ p2 = Path {
    pathString = BS.unpack p
  , pathByteString = p
  }
  where
    p = pathByteString p1 `BS.append` pathByteString p2

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

-- removing prefix
(<\>) :: Path -> Path -> Path
p1 <\> p2 = Path {
    pathString = BS.unpack p
  , pathByteString = p
  }
  where
    p1' = pathByteString p1
    p2' = pathByteString p2
    p = BS.drop (BS.length p2') p1'

-- adding suffix
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

breakSep :: Path -> (Path,Path)
breakSep p = (fromByteString r1, fromByteString r2)
  where
    p' = pathByteString p
    (r1,r2) = BS.breakByte pathSep p'

concatByteString :: [ByteString] -> ByteString
concatByteString = BB.toByteString 
                 . foldr mappend mempty 
                 . map BB.fromByteString
