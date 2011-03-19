module Network.Wai.Application.Classic.Utils where

import Control.Applicative
import Network.Socket (getNameInfo, SockAddr, NameInfoFlag(..))
import Data.Maybe
import Data.List (isPrefixOf)

{-|
  A type for IP address in numeric string representation.
-}
type NumericAddress = String

{-|
  Convert 'SockAddr' to 'NumericAddress'. If the address is
  an IPv4-embedded IPv6 address, the IPv4 is extracted.
-}
getPeerAddr :: SockAddr -> IO NumericAddress
getPeerAddr sa = strip . fromJust . fst <$> getInfo sa
  where
    getInfo = getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True
    strip x
      | "::ffff:" `isPrefixOf` x = drop 7 x
      | otherwise                = x

