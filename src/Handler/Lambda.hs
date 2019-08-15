{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Lambda where
import           Lambda.Endpoint
import           Data.Aeson
import qualified Data.Text          as T 
import           Import
import           Lambda.Trace

data HeartbeatReply =
  HeartbeatReply
    { status :: Text
    , time   :: UTCTime
    }
  deriving (Generic, Show)

instance ToJSON HeartbeatReply where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON HeartbeatReply

getHeartbeatR :: Handler Value
getHeartbeatR = withVerification $ do
  t <- liftIO getCurrentTime
  return $ toJSON $ HeartbeatReply "alive" t

getTraceR :: Handler Value
getTraceR = withVerification $ do
  m <- runDB $ do
    post <- selectList [] []
    return $ fmap toInfo ((fmap entityVal post) :: [BPF])
  n <- runDB $ do
    post <- selectList [] []
    return $ fmap toInfo ((fmap entityVal post) :: [STap])
  return $ toJSONList $ m <> n

putPutTraceR :: Handler Text
putPutTraceR = withVerification $ do
  body <- requireCheckJsonBody :: Handler TraceInfo
  result <- case (traceType body) of
      "BPF" -> do 
        db <- runDB $ insert ((fromInfo body) :: BPF)
        return $ T.pack $ show db
      "STAP" -> do
        db <- runDB $ insert ((fromInfo body) :: STap)
        return $ T.pack $ show db
      _ -> return "failure"
  return result





