{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Utils where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Word
import Network.Socket (getNameInfo, SockAddr, NameInfoFlag(..))

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

pathSep :: Word8
pathSep = 47

hasTrailingPathSeparator :: ByteString -> Bool
hasTrailingPathSeparator "" = False
hasTrailingPathSeparator path
  | BS.last path == pathSep = True
  | otherwise               = False

infixr +++

(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append

(</>) :: ByteString -> ByteString -> ByteString
s1 </> s2
  | hasTrailingPathSeparator s1 = s1 +++ s2
  | otherwise                   = s1 +++ (pathSep `BS.cons` s2)
