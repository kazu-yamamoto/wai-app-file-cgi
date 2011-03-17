module Network.Wai.Application.MaybeIter where

import Control.Monad (mplus)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Enumerator (Iteratee)
import Network.Wai

----------------------------------------------------------------

type MaybeIter a = Iteratee ByteString IO (Maybe a)
type MRsp = MaybeIter Ctl

data CtlBody = CtlNone | CtlBody BL.ByteString | CtlFile FilePath Integer
data Ctl = Ctl Status ResponseHeaders CtlBody

type Rsp = Iteratee ByteString IO Ctl

----------------------------------------------------------------

runAny :: [MRsp] -> Iteratee ByteString IO Ctl
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

(|||) :: Maybe Status -> Maybe Status -> Maybe Status
(|||) = mplus

----------------------------------------------------------------

just :: Monad m => a -> m (Maybe a)
just = return . Just

nothing :: Monad m => m (Maybe a)
nothing = return Nothing
