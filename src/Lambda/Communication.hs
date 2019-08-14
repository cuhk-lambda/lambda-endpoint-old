{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.Communication where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.ByteString.Base64.Lazy  as B64
import qualified Data.ByteString.Lazy         as BS
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.ByteString.Lazy.Char8   as BS8
import           GHC.Generics
import           Network.HTTP.Simple
import           Settings.Lambda
import           System.IO
import           System.Process
import           System.Exit

data SubmitInfo =
  Info
    { trace  :: String
    , status :: String
    }
  deriving (Generic, Show)

instance ToJSON SubmitInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON SubmitInfo

submit :: String -> Handle -> Handle -> ProcessHandle -> IO ()
submit x hout herr process = do
  submitStart x
  runSubmit x hout herr process 0

submitStart :: String -> IO ()
submitStart x = do
  request <- parseRequest $ "PUT " <> platformUrl <> "/submit"
  res <- httpLBS $ setRequestBodyJSON (Info x "start") request
  BS8.putStrLn $ getResponseBody res
  return ()

submitFinished :: String -> Handle -> ProcessHandle -> IO ()
submitFinished x herr process = do
  body <- buildFinishedBody x herr process
  request <- parseRequest $ "PUT " <> platformUrl <> "/submit"
  res <- httpLBS $ setRequestBodyLBS body $ setRequestHeader "Content-Type" ["application/json"] request
  BS8.putStrLn $ getResponseBody res <> "\nfinished"
  return ()

runSubmit :: String -> Handle -> Handle -> ProcessHandle -> Int -> IO ()
runSubmit identity hout herr process chunkNo = do
  stateClose <- hIsClosed hout
  stateEOF <- hIsEOF hout
  if stateClose || stateEOF
    then do
      submitFinished identity herr process
      return ()
    else do
      field <- BS.hGet hout submitChunkSize
      jsonData <- buildProgressBody identity field chunkNo
      request <- parseRequest $ "PUT " <> platformUrl <> "/submit"
      res <-
        httpLBS $
        setRequestBodyLBS jsonData $
        setRequestHeader "Content-Type" ["application/json"] request
      BS8.putStrLn $ getResponseBody res
      runSubmit identity hout herr process (chunkNo + 1)

buildProgressBody :: String -> BS.ByteString -> Int -> IO (BS.ByteString)
buildProgressBody identity field chunkNo =
  return $
  BS.toLazyByteString $
  "{\"trace\":\"" <>
  BS.stringUtf8 identity <>
  "\", \"no\":" <>
  BS.intDec chunkNo <>
  ", \"field\":\"" <> BS.lazyByteString (B64.encode field) <> "\"}"

buildFinishedBody :: String -> Handle -> ProcessHandle -> IO (BS.ByteString)
buildFinishedBody identity herr process = do
  err <- BS.hGetContents herr
  code <- getProcessExitCode process
  BS8.putStrLn err
  return $ BS.toLazyByteString $
    "{\"trace\":\"" <>
    BS.stringUtf8 identity <>
    "\", \"stderr\": \"" <>
    (BS.lazyByteString $ err) <>
    "\", \"code\": " <>
    (BS.intDec $ conv code) <>
    ", \"status\": \"finished\"}"
  
  where
    conv :: Maybe ExitCode -> Int
    conv (Just ExitSuccess) = 0
    conv (Just (ExitFailure x)) = x
    conv _ = 114514
