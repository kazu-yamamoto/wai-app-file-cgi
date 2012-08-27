{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Application.Classic.EventSource (
  toResponseEventSource
) where

import Blaze.ByteString.Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.List as CL

lineBreak :: ByteString -> Int -> Maybe Int
lineBreak bs n = go
  where
    len = BS.length bs
    go | n >= len = Nothing
       | otherwise = case bs `BS.index` n of
                         13 -> go' (n+1)
                         10 -> Just (n+1)
                         _ -> Nothing
    go' n' | n' >= len = Just n'
           | otherwise = case bs `BS.index` n' of
                             10 -> Just (n'+1)
                             _ -> Just n'

-- splitDoubleLineBreak "aaa\n\nbbb" == ["aaa\n\n", "bbb"]
-- splitDoubleLineBreak "aaa\n\nbbb\n\n" == ["aaa\n\n", "bbb\n\n", ""]
-- splitDoubleLineBreak "aaa\r\n\rbbb\n\r\n" == ["aaa\r\n\r", "bbb\n\r\n", ""]
-- splitDoubleLineBreak "aaa" == ["aaa"]
-- splitDoubleLineBreak "" == [""]
splitDoubleLineBreak :: ByteString -> [ByteString]
splitDoubleLineBreak str = go str 0
  where
    go bs n | n < BS.length str =
                    case lineBreak bs n of
                        Nothing -> go bs (n+1)
                        Just n' ->
                            case lineBreak bs n' of
                                Nothing -> go bs (n+1)
                                Just n'' ->
                                    let (xs,ys) = BS.splitAt n'' bs
                                    in xs:go ys 0
            | otherwise = [bs]

eventSourceConduit :: Conduit ByteString (ResourceT IO) (Flush Builder)
eventSourceConduit = CL.concatMapAccum f ""
  where
    f input rest = (last xs, concatMap addFlush $ init xs)
      where
        addFlush x = [Chunk (fromByteString x), Flush]
        xs = splitDoubleLineBreak (rest `BS.append` input)

-- insert Flush if exists a double line-break
toResponseEventSource :: ResumableSource (ResourceT IO) ByteString
                      -> (ResourceT IO) (Source (ResourceT IO) (Flush Builder))
toResponseEventSource rsrc = do
    (src,_) <- unwrapResumable rsrc
    return $ src $= eventSourceConduit
