{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Lang (parseLang) where

import Control.Applicative hiding (many, optional)
import Data.Attoparsec (Parser, takeWhile, parse, feed, Result(..))
import Data.Attoparsec.Char8 (char, string, count, many, space, digit, option, sepBy1)
import Data.ByteString.Char8 hiding (map, count, take, takeWhile, notElem)
import Data.List (sortBy)
import Data.Ord
import Prelude hiding (takeWhile)

parseLang :: ByteString -> [ByteString]
parseLang bs = case feed (parse acceptLanguage bs) "" of
    Done _ ls -> map fst $ sortBy detrimental ls
    _         -> []
  where
    detrimental = flip (comparing snd)

----------------------------------------------------------------

acceptLanguage :: Parser [(ByteString,Int)]
acceptLanguage = rangeQvalue `sepBy1` (spaces *> char ',' *> spaces)

rangeQvalue :: Parser (ByteString,Int)
rangeQvalue = (,) <$> languageRange <*> quality

languageRange :: Parser ByteString
languageRange = takeWhile (`notElem` [32, 44, 59])

quality :: Parser Int
quality = option 1000 (string ";q=" *> qvalue)

qvalue :: Parser Int
qvalue = 1000  <$  (char '1' *> optional (char '.' *> range 0 3 digit))
     <|> read3 <$> (char '0' *> option "0" (char '.' *> range 0 3 digit))
  where
    read3 n = read . take 3 $ n ++ repeat '0'
    optional p = () <$ p <|> return ()

----------------------------------------------------------------

range :: Int -> Int -> Parser a -> Parser [a]
range n m p = (++) <$> count n p <*> upto (m - n) p

upto :: Int -> Parser a -> Parser [a]
upto 0 _ = return []
upto n p = (:) <$> p <*> upto (n - 1) p <|> return []

spaces :: Parser ()
spaces = () <$ many space
