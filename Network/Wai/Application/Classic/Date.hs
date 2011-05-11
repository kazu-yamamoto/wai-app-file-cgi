{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Date (fromWebDate, toWebDate) where

import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.Wai.Application.Classic.Types

----------------------------------------------------------------

fromWebDate :: ByteString -> Maybe UnixTime
fromWebDate _ = Just (UnixTime 0 0)

----------------------------------------------------------------

toWebDate :: UnixTime -> ByteString
toWebDate (UnixTime days secs) = BS.concat [
      week, ", "
    , day, " ", month, " ", year, " "
    , hh, ":", mm, ":", ss, " "
    , "GMT"
    ]
  where
    (h,m,s) = toHHMMSS secs
    hh = pad h
    mm = pad m
    ss = pad s
    week = weekDays ! w
      where
        -- 1970/1/1 is Thu (4)
        w = (days + 3) `mod` 7 + 1
    day = pad d
    month = months ! m'
    (y,m',d) = toYYMMDD days
    year = toB y
    toB :: Integral i => i -> ByteString
    toB = BS8.pack . show
    to2B :: Integral i => i -> ByteString
    to2B = BS8.pack . ('0' :) . show
    pad n = if n < 10 then to2B n else toB n

toHHMMSS :: Int -> (Int,Int,Int)
toHHMMSS x = (hh,mm,ss)
  where
    (hhmm,ss) = x `divMod` 60
    (hh,mm) = hhmm `divMod` 60

toYYMMDD :: Int -> (Int,Int,Int)
toYYMMDD x = (yy, mm, dd)
  where
    (y,d) = x `divMod` 365
    cy = 1970 + y
    cy' = cy - 1
    leap = cy' `div` 4 - cy' `div` 100 + cy' `div` 400 - 477
    (yy,days) = adjust cy d leap
    (mm,dd) = findMonth 1 monthDays (days + 1)
    adjust ty td aj
      | td >= aj        = (ty, td - aj)
      | isLeap (ty - 1) = if td + 366 >= aj
                          then (ty - 1, td + 366 - aj)
                          else adjust (ty - 1) (td + 366) aj
      | otherwise       = if td + 365 >= aj
                          then (ty - 1, td + 365 - aj)
                          else adjust (ty - 1) (td + 365) aj
    isLeap year = year `mod` 4 == 0
              && (year `mod` 400 == 0 ||
                  year `mod` 100 /= 0)
    monthDays
      | isLeap yy = leapMonthDays
      | otherwise = normalMonthDays
    findMonth _ [] _ = error "findMonth"
    findMonth m (n:ns) z
      | z <= n    = (m,z)
      | otherwise = findMonth (m+1) ns (z-n)

--------------------------------

months :: Array Int ByteString
months = listArray (1,12) [
      "Jan", "Feb", "Mar"
    , "Apr", "May", "Jun"
    , "Jul", "Aug", "Sep"
    , "Oct", "Nov", "Dec"
    ]

weekDays :: Array Int ByteString
weekDays = listArray (1,7) [
      "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
    ]

normalMonthDays :: [Int]
normalMonthDays = [31,28,31,30,31,30,31,31,30,31,30,31]

leapMonthDays :: [Int]
leapMonthDays   = [31,29,31,30,31,30,31,31,30,31,30,31]
