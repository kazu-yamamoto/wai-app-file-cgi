{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Lang (parseLang) where

import Control.Applicative hiding (many, optional)
import Data.Attoparsec.Char8 hiding (take)
import Data.ByteString.Char8 hiding (map, count, take)
import Data.List
import Data.Ord

parseLang :: ByteString -> [String]
parseLang bs = case feed (parse acceptLanguage bs) "" of
    Done _ ls -> map fst $ sortBy detrimental ls
    _         -> []
  where
    detrimental = flip (comparing snd)

----------------------------------------------------------------

acceptLanguage :: Parser [(String,Int)]
acceptLanguage = rangeQvalue `sepBy1` (spaces *> char ',' *> spaces)

rangeQvalue :: Parser (String,Int)
rangeQvalue = (,) <$> languageRange <*> quality

languageRange :: Parser String
languageRange = (++) <$> language <*> sublang

language :: Parser String
language = many1 letter_ascii

sublang :: Parser String
sublang = option "" ((:) <$> char '-' <*> many1 letter_ascii)

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
