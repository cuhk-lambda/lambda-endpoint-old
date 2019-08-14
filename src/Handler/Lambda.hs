{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Lambda where

import           Data.Aeson
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
  return $ toJSON $ HeartbeatReply "alive" t
