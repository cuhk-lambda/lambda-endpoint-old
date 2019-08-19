{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Lambda (
  getHeartbeatR, 
  getTraceR, 
  putPutTraceR, 
  postStartTraceR, 
  postRemoveTraceR,
  getRunningTracesR,
  getRunningTraceR
) where
import           Lambda.Endpoint
import           Data.Aeson
import qualified Data.Text          as T 
import           Import
import           Lambda.Trace 
import           Database.Persist.Sql
import           Lambda.Trace.Unsafe
data RemoveTrace = 
  RemoveTrace {
    removeType :: Text,
    removeId   :: Int64
  }
  deriving (Generic, Show)

data HeartbeatReply =
  HeartbeatReply
    { status :: Text
    , time   :: UTCTime
    }
  deriving (Generic, Show)

data StartTrace = 
  StartTrace {
    traceT :: Text,
    traceId :: Int64,
    lasting :: Int 
  } deriving (Generic, Show)

data StartTraceReply = 
  StartTraceReply {
    thread :: String,
    filePath :: String 
  } deriving (Generic, Show)

data InfoWithId =
  InfoWithId {
    info :: TraceInfo,
    traceNo :: Int64
  } deriving (Generic, Show)

data InfoWithPath =
    InfoWithPath {
      info' :: TraceInfo,
      path :: String
  } deriving (Generic, Show)

instance ToJSON HeartbeatReply where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON HeartbeatReply


instance ToJSON StartTrace where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON StartTrace

instance ToJSON InfoWithId where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON InfoWithId

instance ToJSON StartTraceReply where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON StartTraceReply

instance ToJSON RemoveTrace where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON RemoveTrace

instance ToJSON InfoWithPath where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON InfoWithPath

getHeartbeatR :: Handler Value
getHeartbeatR = withVerification $ do
  t <- liftIO getCurrentTime
  return $ toJSON $ HeartbeatReply "alive" t

getTraceR :: Handler Value
getTraceR = withVerification $ do
  m <- runDB $ do
    post <- selectList [] []
    return $ fmap (pure InfoWithId <*> toInfo . entityVal <*> (unSqlBackendKey . unBPFKey . entityKey)) post
  n <- runDB $ do
    post <- selectList [] []
    return $ fmap (pure InfoWithId <*> toInfo . entityVal <*> (unSqlBackendKey . unSTapKey . entityKey)) post
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
      _ -> do 
        invalidArgs ["traceType"]
  return result

postStartTraceR :: Handler Value
postStartTraceR = withVerification $ do
  body <- requireCheckJsonBody :: Handler StartTrace
  case (traceT body) of
    "BPF" -> do 
      let key = BPFKey (SqlBackendKey $ Handler.Lambda.traceId body)
      db <- runDB $ selectList [BPFId ==. key] [LimitTo 1]
      case db of
        [a] -> do
          (id', path') <- liftIO $ Lambda.Trace.trace (entityVal a) (lasting body)
          return $ toJSON $ StartTraceReply (show id') path' 
        _ -> do
          invalidArgs ["traceId"]
    "STAP" -> do 
      let key = STapKey (SqlBackendKey $ Handler.Lambda.traceId body)
      db <- runDB $ selectList [STapId ==. key] [LimitTo 1]
      case db of
        [a] -> do
          (id', path') <- liftIO $ Lambda.Trace.trace (entityVal a) (lasting body)
          return $ toJSON $ StartTraceReply (show id') path' 
        _ -> do
          invalidArgs ["traceId"]
    _ -> do 
      invalidArgs ["traceType"]

postRemoveTraceR :: Handler Text
postRemoveTraceR = withVerification $ do
  body <- requireCheckJsonBody :: Handler RemoveTrace
  case (removeType body) of
    "BPF" -> do
      let key = BPFKey (SqlBackendKey $ Handler.Lambda.removeId body)
      _ <- runDB $ delete key
      return "deleted"
    "STAP" -> do
      let key = STapKey (SqlBackendKey $ Handler.Lambda.removeId body)
      _ <- runDB $ delete key
      return "deleted"
    _ -> do
      invalidArgs ["removeType"]

getRunningTracesR :: Handler Value
getRunningTracesR = withVerification $ do
  xs <- liftIO getAllRunning
  return $ toJSONList (map (\(x, y)-> InfoWithPath y x) xs)

getRunningTraceR :: Handler Value
getRunningTraceR = withVerification $ do
  parm <- lookupGetParam "path"
  liftIO $ print parm
  case parm of 
    Just a -> do
      i <- liftIO $ getRunningTrace (T.unpack a)
      case i of
        Nothing -> return (toJSON ())
        Just x -> return (toJSON x)
    Nothing ->
      invalidArgs ["path"]
    







