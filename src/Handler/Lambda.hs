{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Lambda where

import Import
import Data.Aeson
data HeartbeatReply = HeartbeatReply {
      status :: Text
    , time  :: UTCTime
} deriving (Generic, Show)

instance ToJSON HeartbeatReply where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON HeartbeatReply


getHeartbeatR :: Handler Value
getHeartbeatR = do
    t <- liftIO getCurrentTime
    return $ toJSON $ HeartbeatReply "alive" t

