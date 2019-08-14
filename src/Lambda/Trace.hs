{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.Trace where

import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as TIO
import           Data.UUID.V4
import qualified Lambda.BPFTrace   as BPFT
import qualified Lambda.SystemTap  as STAP
import           Model             (BPF, STap, bPFEnvironment, bPFOptions,
                                    bPFValue, sTapEnvironment, sTapOptions,
                                    sTapValue)
import           Settings.Lambda
import           System.IO
import           System.Process
import           Lambda.Communication
import           Control.Concurrent
class Trace a where
  generate :: a -> Int -> T.Text
  env :: a -> [(String, String)]
  opt :: a -> [String]
  getExec :: a -> String

instance Trace STap where
  generate = STAP.generate
  env a =
    zipWith (\x y -> (T.unpack x, T.unpack y)) (sTapEnvironment a) (sTapValue a)
  opt = (map T.unpack) . sTapOptions
  getExec = const sTapPath

instance Trace BPF where
  generate = BPFT.generate
  env a =
    zipWith (\x y -> (T.unpack x, T.unpack y)) (bPFEnvironment a) (bPFValue a)
  opt = (map T.unpack) . bPFOptions
  getExec = const bPFPath

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
        , System.Process.env = Just (Lambda.Trace.env a)
        }
  hPutStr hin rootPassword
  hClose hin
  return (hout, herr, p, filePath)

trace :: Trace a => a -> Int -> IO ThreadId
trace a t = do
  (hout, herr, process, path) <- startTrace a t
  forkIO $ submit path hout herr process

