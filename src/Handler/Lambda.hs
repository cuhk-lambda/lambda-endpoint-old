{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Lambda where
import           Settings.Lambda
import           Lambda.Endpoint
import           Data.Aeson
import qualified Data.Text.Encoding as T
import qualified Data.Text          as T 
import           Import
       
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
getHeartbeatR = do
  t <- liftIO getCurrentTime
  sec <- liftIO hashedSecret
  addHeader "Authorization" $ (T.pack endpointUUID) <> ":" <> (T.decodeUtf8 sec)
  return $ toJSON $ HeartbeatReply "alive" t


