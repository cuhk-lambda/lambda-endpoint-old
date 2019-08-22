{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.Trace
  ( trace
  , module Lambda.Trace.Class
  ) where

import           Control.Concurrent
import qualified Data.Text.Lazy.IO    as TIO
import           Data.UUID.V4
import qualified Lambda.Communication as C
import           Lambda.Trace.Class
import           Lambda.Trace.Unsafe
import           Settings.Lambda
import           System.IO
import           System.Process

generateFile :: Trace a => a -> Int -> IO String
generateFile x t = do
  text <- return $ generate x t
  uuid <- nextRandom
  let filePath = "/tmp/lambda-" <> show uuid <> ".trace"
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
  return (hout, herr, p, filePath)

trace :: Trace a => a -> Int -> IO (ThreadId, FilePath)
trace a t = do
  (hout, herr, process', path) <- startTrace a t
  tid <- forkIO $ C.submit path hout herr process'
  putRunningTrace
    path
    (tid, (Nothing, Just hout, Just herr, process'), toInfo a)
  return (tid, path)
