{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Date (parseDate, utcToDate) where

import Data.Array
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time
import Data.Time.Calendar.WeekDate
import Locale hiding (months)

----------------------------------------------------------------

parseDate :: ByteString -> Maybe UTCTime
parseDate bs = rfc1123Date cs `mplus` rfc850Date cs `mplus` asctimeDate cs
  where
    cs = BS8.unpack bs

----------------------------------------------------------------

rfc1123Format :: String
rfc1123Format = "%a, %d %b %Y %H:%M:%S GMT"

rfc850Format :: String
rfc850Format  = "%A, %d-%b-%y %H:%M:%S GMT"

-- xxx: allows "Nov 6" as well as "Nov  6", sigh.
asctimeFormat :: String
asctimeFormat = "%a %b %e %H:%M:%S %Y"

preferredFormat :: String
preferredFormat = rfc1123Format

----------------------------------------------------------------

rfc1123Date :: String -> Maybe UTCTime
rfc1123Date = parseTime defaultTimeLocale preferredFormat

rfc850Date :: String -> Maybe UTCTime
rfc850Date str = parseTime defaultTimeLocale rfc850Format str >>= y2k
  where
    y2k utct = let (y,m,d) = toGregorian $ utctDay utct
               in if y < 1950
                  then Just utct { utctDay = fromGregorian (y+100) m d }
                  else Just utct

asctimeDate :: String -> Maybe UTCTime
asctimeDate = parseTime defaultTimeLocale asctimeFormat

----------------------------------------------------------------

utcToDate :: UTCTime -> ByteString
utcToDate utcTime = BS.concat [
      week, ", "
    , day, " ", month, " ", year, " "
    , hh, ":", mm, ":", ss, " "
    , "GMT"
    ]
  where
    (y,m,d) = toGregorian (utctDay utcTime)
    (_,_,w)       = toWeekDate (utctDay utcTime)
    toB :: Integral i => i -> ByteString
    toB = BS8.pack . show
    to2B :: Integral i => i -> ByteString
    to2B = BS8.pack . ('0' :) . show
    year =  toB $ y
    month = months ! m
    week = weekDays ! w
    pad n = if n < 10 then to2B n else toB n
    day = pad d
    tod = timeToTimeOfDay . utctDayTime $ utcTime
    hh = pad . todHour $ tod
    mm = pad . todMin $ tod
    ss' :: Int
    ss' = floor . todSec $ tod
    ss = pad ss'

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
