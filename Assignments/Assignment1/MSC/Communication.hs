module MSC.Communication where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Network
import           System.IO

import           MSC.System


sendMessage :: MSCLoc -> String -> IO ()
sendMessage port msg = do
  hndl <- connectTo "localhost" (PortNumber $ fromIntegral port)
  hSetBuffering hndl NoBuffering
  hPutStrLn hndl msg
  hFlush hndl
  hClose hndl
