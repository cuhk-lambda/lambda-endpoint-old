{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Lambda.Trace where
import qualified Data.Text.Lazy as T
import qualified Lambda.SystemTap as STAP
import qualified Lambda.BPFTrace as BPFT
import Settings.Lambda
import qualified Data.Text.Lazy.IO as TIO
import Data.UUID.V4
import System.IO
import System.Process
import Model (
    STap
    , BPF
    , sTapEnvironment
    , sTapValue
    , sTapOptions
    , bPFEnvironment
    , bPFValue
    , bPFOptions
    )

class Trace a where
    generate :: a -> Int -> T.Text
    env :: a -> [(String, String)]
    opt :: a -> [String]
    getExec :: a -> String

instance Trace STap where
    generate = STAP.generate
    env a = zipWith (\x y -> (T.unpack x, T.unpack y)) (sTapEnvironment a) (sTapValue a)
    opt = (map T.unpack) . sTapOptions
    getExec = const sTapPath

instance Trace BPF where
    generate = BPFT.generate
    env a = zipWith (\x y -> (T.unpack x, T.unpack y)) (bPFEnvironment a) (bPFValue a)
    opt = (map T.unpack) . bPFOptions
    getExec = const bPFPath

generateFile :: Trace a => a -> Int -> IO String
generateFile x t = do
    text <- return $ generate x t
    uuid <- nextRandom
    let filePath = "/tmp/lambda-" <> show uuid <> ".stap"
    withFile filePath WriteMode (`TIO.hPutStr` text)
    return filePath

startTrace :: Trace a => a -> Int -> IO (Handle, ProcessHandle, String)
startTrace a t =  do
    filePath <- generateFile a t
    (Just hin, Just hout, _, p) <-
        createProcess (proc "sudo" (["-S", (getExec a), filePath] ++ (opt a))) {
            std_in = CreatePipe,
            std_out = CreatePipe, 
            System.Process.env = Just (Lambda.Trace.env a)
        }
    hPutStr hin rootPassword
    hClose hin
    return (hout, p, filePath)


        




    

