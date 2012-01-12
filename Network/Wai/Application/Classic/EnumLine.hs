{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.EnumLine (head) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 () -- for OverloadedStrings
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 () -- for OverloadedStrings
import Data.Conduit
import Prelude hiding (head)

head :: Sink BS.ByteString IO (Maybe BS.ByteString)
head = line id False

line :: Builder -> Bool
     -> Sink BS.ByteString IO (Maybe BS.ByteString)
line = undefined
{-
line build dropLF = continue go
  where
    go (Chunks cnk) = breakLine (BL.fromChunks cnk)
    go EOF = yield Nothing EOF
    breakLine "" = line build dropLF
    breakLine xs | dropLF && y == lf = yield (Just ln) (Chunks cnk)
      where
        y = BL.head xs
        ys = BL.tail xs
        ln = toStrict . build $ ""
        cnk = BL.toChunks ys
    breakLine xs = case BL.break eol xs of
        (ys, "") -> line (build +++ toB ys) False
        (ys, zs) -> dropCRLF ys zs
    dropCRLF xs ys
      | z == cr    = dropCR xs zs
      | otherwise  = yield (Just ln) (Chunks cnks) -- dropLF
      where
        z  = BL.head ys
        zs = BL.tail ys
        ln = toStrict . build $ xs
        cnks = BL.toChunks zs
    dropCR xs ""  = line (build +++ toB xs) True
    dropCR xs ys
      | z == lf   = yield (Just ln) (Chunks czs)
      | otherwise = yield (Just ln) (Chunks cys)
      where
        z  = BL.head ys
        zs = BL.tail ys
        ln = toStrict . build $ xs
        cys = BL.toChunks ys
        czs = BL.toChunks zs
    lf = 10
    cr = 13
    eol = (`elem` [lf, cr])
-}

type Builder = BL.ByteString -> BL.ByteString

(+++) :: Builder -> Builder -> Builder
a +++ b = a . b

toB :: BL.ByteString -> Builder
toB b = (b `BL.append`)

toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks
