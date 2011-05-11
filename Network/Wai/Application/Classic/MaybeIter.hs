module Network.Wai.Application.Classic.MaybeIter where

import Control.Monad (mplus)
import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee)
import Network.Wai.Application.Classic.Types

----------------------------------------------------------------

type MaybeIter a = Iteratee ByteString IO (Maybe a)
type MRsp = MaybeIter RspSpec

type Rsp = Iteratee ByteString IO RspSpec

----------------------------------------------------------------

runAny :: [MRsp] -> Iteratee ByteString IO RspSpec
runAny [] = error "runAny"
runAny (a:as) = do
    mrsp <- a
    case mrsp of
      Nothing  -> runAny as
      Just rsp -> return rsp

runAnyMaybe :: [MRsp] -> MRsp
runAnyMaybe []     = nothing
runAnyMaybe (a:as) = do
    mx <- a
    case mx of
      Nothing -> runAnyMaybe as
      Just _  -> return mx

----------------------------------------------------------------

infixr 5 >>|, |>|, |||

(>>|) :: Maybe a -> (a -> MaybeIter b) -> MaybeIter b
v >>| act =
    case v of
      Nothing -> nothing
      Just x  -> act x
      
(|>|) :: MaybeIter a -> (a -> MaybeIter b) -> MaybeIter b
a |>| act = do
    v <- a
    case v of
      Nothing -> nothing
      Just x  -> act x

(|||) :: Maybe a -> Maybe a -> Maybe a
(|||) = mplus

----------------------------------------------------------------

just :: Monad m => a -> m (Maybe a)
just = return . Just

nothing :: Monad m => m (Maybe a)
nothing = return Nothing
