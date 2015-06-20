{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Application.Classic.Range (skipAndSize) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>), (<$), (<*), (*>))
#endif
import Control.Applicative ((<|>), many)
import Data.Attoparsec.ByteString hiding (satisfy)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Types

-- |
-- >>> skipAndSize "bytes=0-399" 10000
-- Just (0,400)
-- >>> skipAndSize "bytes=500-799" 10000
-- Just (500,300)
-- >>> skipAndSize "bytes=-500" 10000
-- Just (9500,500)
-- >>> skipAndSize "bytes=9500-" 10000
-- Just (9500,500)
skipAndSize :: ByteString -> Integer -> Maybe (Integer,Integer)
skipAndSize bs size = case parseRange bs of
  Just [rng] -> adjust rng size
  _          -> Nothing

adjust :: ByteRange -> Integer -> Maybe (Integer,Integer)
adjust (ByteRangeFromTo beg end) siz
  | beg <= end && end <= siz     = Just (beg, end - beg + 1)
  | otherwise                    = Nothing
adjust (ByteRangeFrom beg) siz
  | beg <= siz                   = Just (beg, siz - beg)
  | otherwise                    = Nothing
adjust (ByteRangeSuffix end) siz
  | end <= siz                   = Just (siz - end, end)
  | otherwise                    = Nothing

parseRange :: ByteString -> Maybe [ByteRange]
parseRange bs = case parseOnly byteRange bs of
    Right x -> Just x
    _       -> Nothing

byteRange :: Parser [ByteRange]
byteRange = string "bytes=" *> (ranges <* endOfInput)

ranges :: Parser [ByteRange]
ranges = sepBy1 (range <|> suffixRange) (spcs >> char ',' >> spcs)

range :: Parser ByteRange
range = do
  beg <- num <* char '-'
  (ByteRangeFromTo beg <$> num) <|> return (ByteRangeFrom beg)

suffixRange :: Parser ByteRange
suffixRange = ByteRangeSuffix <$> (char '-' *> num)

num :: Parser Integer
num = read <$> many1 digit

spcs :: Parser ()
spcs = () <$ many spc

spc :: Parser Char
spc = satisfy (`B8.elem` " \t")
