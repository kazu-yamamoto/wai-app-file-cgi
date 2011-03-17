{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Application.Utils where

import Data.ByteString.Lazy.Char8 ()
import Data.Enumerator (($$))
import Data.Enumerator (Iteratee,Enumeratee,joinI)

----------------------------------------------------------------

infixr 0 =$

(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
ee =$ ie = joinI $ ee $$ ie

----------------------------------------------------------------

infixr 6 $.

($.) :: (a -> b) -> a -> b
($.) = ($)

