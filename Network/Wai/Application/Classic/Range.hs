{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Range (skipAndSize) where

import Control.Applicative hiding (many,optional)
import Data.Attoparsec.Char8 hiding (take)
import Data.ByteString.Char8 hiding (map, count, take, elem)

skipAndSize :: ByteString -> Integer -> Maybe (Integer,Integer)
skipAndSize bs size = case parseRange bs of
  Just [(mbeg,mend)] -> adjust mbeg mend size
  _                  -> Nothing

adjust :: Maybe Integer -> Maybe Integer -> Integer -> Maybe (Integer,Integer)
adjust (Just beg) (Just end) siz
  | beg <= end && end <= siz     = Just (beg, end - beg + 1)
  | otherwise                    = Nothing
adjust (Just beg) Nothing    siz
  | beg <= siz                   = Just (beg, siz - beg)
  | otherwise                    = Nothing
adjust Nothing    (Just end) siz
  | end <= siz                   = Just (siz - end, end)
  | otherwise                    = Nothing
adjust Nothing    Nothing    _   = Nothing

type Range = (Maybe Integer, Maybe Integer)

parseRange :: ByteString -> Maybe [Range]
parseRange bs = case feed (parse byteRange bs) "" of
    Done _ x -> Just x
    _        -> Nothing

byteRange :: Parser [Range]
byteRange = string "bytes=" *> (ranges <* endOfInput)

ranges :: Parser [Range]
ranges = sepBy1 (range <|> suffixRange) (spcs >> char ',' >> spcs)

range :: Parser Range
range = (,) <$> ((Just <$> num) <* char '-')
            <*> option Nothing (Just <$> num)

suffixRange :: Parser Range
suffixRange = (,) Nothing <$> (char '-' *> (Just <$> num))

num :: Parser Integer
num = read <$> many1 digit

spcs :: Parser ()
spcs = () <$ many spc

spc :: Parser Char
spc = satisfy (`elem` " \t")
