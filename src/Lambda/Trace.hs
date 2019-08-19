{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric    #-}
module Lambda.Trace(
  trace,
  module Lambda.Trace.Class
) where

import qualified Data.Text.Lazy.IO as TIO
import           Data.UUID.V4         
import           Settings.Lambda
import           System.IO
import           System.Process
import qualified Lambda.Communication as C
import           Control.Concurrent
import           Lambda.Trace.Class
import           Lambda.Trace.Unsafe
generateFile :: Trace a => a -> Int -> IO String
generateFile x t = do
  text <- return $ generate x t
  uuid <- nextRandom
  let filePath = "/tmp/lambda-" <> show uuid <> ".stap"
  withFile filePath WriteMode (`TIO.hPutStr` text)
  return filePath

startTrace :: Trace a => a -> Int -> IO (Handle, Handle, ProcessHandle, String)
startTrace a t = do
  filePath <- generateFile a t
  (Just hin, Just hout, Just herr, p) <-
    createProcess
      (proc "sudo" (["-S", (getExec a), filePath] ++ (opt a)))
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , System.Process.env = Just (Lambda.Trace.Class.env a)
        }
  hPutStr hin rootPassword
  hClose hin
  putRunningTrace filePath (toInfo a)
  return (hout, herr, p, filePath)

trace :: Trace a => a -> Int -> IO (ThreadId, FilePath)
trace a t = do
  (hout, herr, process', path) <- startTrace a t
  tid <- forkIO $ C.submit path hout herr process'
  return (tid, path)






