{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.Communication where


import           Data.Aeson
import qualified Data.ByteString.Base64.Lazy  as B64
import qualified Data.ByteString.Lazy         as BS
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.ByteString.Lazy.Char8   as BS8
import qualified Data.ByteString.Char8        as BSS
import           GHC.Generics
import           Network.HTTP.Simple
import           Settings.Lambda
import           System.IO
import           System.Process
import           System.Exit
import           Lambda.Endpoint
import           Lambda.Trace.Unsafe
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
  request <- parseRequest $ "PUT " <> platformUrl-- <> "/submit"
  hash <- hashedSecret
  _ <- httpLBS 
    $ setRequestBodyJSON (Info x "start") 
    $ setRequestHeader "Authorization" [BSS.append (BSS.pack endpointUUID) hash] request
  return ()

submitFinished :: String -> Handle -> ProcessHandle -> IO ()
submitFinished x herr process = do
  body <- buildFinishedBody x herr process
  request <- parseRequest $ "PUT " <> platformUrl-- <> "/submit"
  hash <- hashedSecret
  _ <- httpLBS 
    $ setRequestBodyLBS body $ setRequestHeader "Content-Type" ["application/json"] 
    $ setRequestHeader "Authorization" [BSS.append (BSS.pack endpointUUID) hash] request
  delRunningTrace x
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
      request <- parseRequest $ "PUT " <> platformUrl-- <> "/submit"
      hash <- hashedSecret
      _ <-
        httpLBS $
        setRequestBodyLBS jsonData $
        setRequestHeader "Content-Type" ["application/json"] $
        setRequestHeader "Authorization" [BSS.append (BSS.pack endpointUUID) hash] request
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
  return $ BS.toLazyByteString $
    "{\"trace\":\"" <>
    BS.stringUtf8 identity <>
    "\", \"stderr\": \"" <>
    (BS.lazyByteString $ BS.intercalate "\\n" $ BS8.split '\n' $ BS.intercalate "\\\"" $ BS8.split '\"' err) <>
    "\", \"code\": " <>
    (BS.intDec $ conv code) <>
    ", \"status\": \"finished\"}"
  
  where
    conv :: Maybe ExitCode -> Int
    conv (Just ExitSuccess) = 0
    conv (Just (ExitFailure x)) = x
    conv _ = 114514
