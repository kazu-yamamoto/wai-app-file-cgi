{-# LANGUAGE ForeignFunctionInterface #-}

module SendFile where

import Data.Int
import Data.Word
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Storable (poke)
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..))
import Foreign.C.Error
import Control.Concurrent

sendfile :: Socket -> FilePath -> Integer -> Integer -> IO ()
sendfile sock path off len = do
    fd <- openFd path ReadOnly Nothing defaultFileFlags
    let len' = fromInteger len :: Word64
    sendfileI (Fd $ fdSocket sock) fd len'
    closeFd fd

sendfileI :: Fd -> Fd -> Word64 -> IO ()
sendfileI out_fd in_fd bytes = do
    siz <- c_sendfile out_fd in_fd nullPtr bytes
    if siz < 0
       then do
          errno <- getErrno
          if errno == eAGAIN
             then do
               threadWaitWrite out_fd
               sendfileI out_fd in_fd bytes
             else return ()
       else let siz' = fromIntegral siz
            in if siz' /= bytes
                  then sendfileI out_fd in_fd (bytes - siz')
                  else return ()

foreign import ccall unsafe "sendfile64" c_sendfile
    :: Fd -> Fd -> Ptr (Int64) -> (Word64) -> IO (Int64)
