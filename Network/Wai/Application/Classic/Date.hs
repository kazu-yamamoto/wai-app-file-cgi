module Network.Wai.Application.Classic.Date (parseDate, utcToDate) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Locale
import Control.Monad

-- xxx: ClockTime -> UTCTime
----------------------------------------------------------------

parseDate :: ByteString -> Maybe UTCTime
parseDate bs = rfc1123Date cs `mplus` rfc850Date cs `mplus` asctimeDate cs
  where
    cs = BS.unpack bs

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
utcToDate = BS.pack . formatTime defaultTimeLocale preferredFormat
