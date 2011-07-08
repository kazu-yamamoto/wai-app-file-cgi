{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Classic.Utils (
    NumericAddress, showSockAddr
  , hasTrailingPathSeparator, pathSep
  , (+++), (</>)
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import Data.Word
import Network.Socket (SockAddr(..))
import System.ByteOrder

{-|
  A type for IP address in numeric string representation.
-}
type NumericAddress = String

toHostByteOrder :: Word32 -> [Word32]
toHostByteOrder w32
  | byteOrder == LittleEndian = [w1,w2,w3,w4]
  | otherwise                 = [w4,w3,w2,w1]
  where
    t1 = w32
    t2 = shift t1 (-8)
    t3 = shift t2 (-8)
    t4 = shift t3 (-8)
    w1 = t1 .&. 0x000000ff
    w2 = t2 .&. 0x000000ff
    w3 = t3 .&. 0x000000ff
    w4 = t4 .&. 0x000000ff

showIPv4 :: Word32 -> NumericAddress
showIPv4 w32 = show w1 ++ "." ++ show w2 ++ "." ++ show w3 ++ "." ++ show w4
  where
    [w1,w2,w3,w4] = toHostByteOrder w32

{-|
  Convert 'SockAddr' to 'NumericAddress'. If the address is
  an IPv4-embedded IPv6 address, the IPv4 is extracted.
-}
showSockAddr :: SockAddr -> NumericAddress
showSockAddr (SockAddrInet _ w32) = showIPv4 w32
showSockAddr (SockAddrInet6 _ _ (0,0,0x0000ffff,w32) _) = showIPv4 w32
showSockAddr _ = "::1" -- FIXME

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
