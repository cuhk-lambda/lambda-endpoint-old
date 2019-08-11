{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Lambda.Communication where
import System.IO
import Settings.Lambda
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import Control.Concurrent
import Data.Aeson
import GHC.Generics

data SubmitInfo = Info {
    trace :: String
,   status :: String
} deriving (Generic, Show)
instance ToJSON SubmitInfo where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SubmitInfo

submit :: String -> Handle -> IO ThreadId
submit x handle = do
    submitStart x
    forkIO $ runSubmit x handle

submitStart :: String -> IO ()
submitStart x = do
    request <- parseRequest "POST http://httpbin.org/post"
    res <- httpLBS $ 
        setRequestBodyJSON (Info x "start") request
    BS.putStrLn $ getResponseBody res
    return ()

submitFinished :: String -> IO ()
submitFinished x = do
    request <- parseRequest "POST http://httpbin.org/post"
    res <- httpLBS $ 
        setRequestBodyJSON (Info x "finished") request
    BS.putStrLn $ getResponseBody res
    return ()

runSubmit :: String -> Handle -> IO ()
runSubmit identity handle  = do
    stateClose <- hIsClosed handle
    stateEOF   <- hIsEOF handle
    if stateClose || stateEOF 
    then do
        submitFinished identity
        return ()
    else do
        field <- BS.hGet handle submitChunkSize
        jsonData  <- buildProgressBody identity field
        request <- parseRequest "POST http://httpbin.org/post"
        res <- httpLBS $ setRequestBodyLBS jsonData $ setRequestHeader "Content-Type" ["application/json"] request
        BS.putStrLn $ getResponseBody res
        runSubmit identity handle

buildProgressBody :: String -> BS.ByteString -> IO (BS.ByteString)
buildProgressBody identity field = return $ BS.toLazyByteString $
    "{\"trace\":\"" <> BS.stringUtf8 identity <> "\", \"field\":\""  <> BS.lazyByteString (B64.encode field) <> "\"}"