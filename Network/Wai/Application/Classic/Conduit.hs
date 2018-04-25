{-# LANGUAGE OverloadedStrings, CPP #-}

module Network.Wai.Application.Classic.Conduit (
    byteStringToBuilder
  , toResponseSource
  , parseHeader
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB (byteString)
import Data.CaseInsensitive (CI(..), mk)
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import Data.Word
import Network.HTTP.Types

----------------------------------------------------------------

byteStringToBuilder :: ByteString -> Builder
byteStringToBuilder = BB.byteString

----------------------------------------------------------------

#if MIN_VERSION_conduit(1,3,0)
toResponseSource :: SealedConduitT () ByteString IO ()
                 -> IO (ConduitT () (Flush Builder) IO ())
toResponseSource rsrc = do
    let src = unsealConduitT rsrc
    return $ src .| CL.map (Chunk . byteStringToBuilder)
#else
toResponseSource :: ResumableSource IO ByteString
                 -> IO (Source IO (Flush Builder))
toResponseSource rsrc = do
    (src,_) <- unwrapResumable rsrc
    return $ src $= CL.map (Chunk . byteStringToBuilder)
#endif

----------------------------------------------------------------

parseHeader :: ConduitM ByteString o IO RequestHeaders
parseHeader = sinkParser parseHeader'

parseHeader' :: Parser RequestHeaders
parseHeader' = stop <|> loop
  where
    stop = [] <$ (crlf <|> endOfInput)
    loop = (:) <$> keyVal <*> parseHeader'

type RequestHeader = (CI ByteString, ByteString)

keyVal :: Parser RequestHeader
keyVal = do
    key <- takeTill (wcollon==)
    _ <- word8 wcollon
    skipWhile (wspace ==)
    val <- takeTill (`elem` [wlf,wcr])
    crlf
    return (mk key, val)

crlf :: Parser ()
crlf = (cr >> (lf <|> return ())) <|> lf

cr :: Parser ()
cr = () <$ word8 wcr

lf :: Parser ()
lf = () <$ word8 wlf

wcollon :: Word8
wcollon = 58

wcr :: Word8
wcr = 13

wlf :: Word8
wlf = 10

wspace :: Word8
wspace = 32
