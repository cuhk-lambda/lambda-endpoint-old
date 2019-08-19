{-# LANGUAGE OverloadedStrings #-}
module Settings.Lambda.Unsafe where
import Data.Aeson
import Data.Yaml
import Data.IORef
import System.IO.Unsafe
data LambdaGlobalInfo = LambdaGlobalInfo {
    getRootPassword :: {-# UNPACK #-} !String,
    getBPFPath :: {-# UNPACK #-} !String,
    getSTapPath :: {-# UNPACK #-} !String,
    getSubmitChunkSize :: {-# UNPACK #-} !Int,
    getPlatformUrl :: {-# UNPACK #-} !String,
    getSecret :: {-# UNPACK #-} !String,
    getUUID :: {-#UNPACK#-} !String
} 

instance FromJSON LambdaGlobalInfo where
    parseJSON = withObject "LambdaGlobalInfo" $ \v -> LambdaGlobalInfo
        <$> v .: "root-password"
        <*> v .: "bpf-path"
        <*> v .: "stap-path"
        <*> v .: "submit-chunk-size"
        <*> v .: "platform-url"
        <*> v .: "secret"
        <*> v .: "endpoint-uuid"

instance ToJSON LambdaGlobalInfo where
    -- this generates a Value
    toJSON (LambdaGlobalInfo rootPassword'  bPFPath' sTapPath' submitChunkSize' platformUrl' secret' uuid') =
        object [
            "root-password" .= rootPassword', 
            "bpf-path" .= bPFPath', 
            "stap-path" .= sTapPath', 
            "submit-chunk-size" .= submitChunkSize',
            "platform-url" .= platformUrl',
            "secret" .= secret',
            "endpoint-uuid" .= uuid'
        ]
        
    -- this encodes directly to a bytestring Builder
    toEncoding (LambdaGlobalInfo rootPassword'  bPFPath' sTapPath' submitChunkSize' platformUrl' secret' uuid') =
        pairs (
            "root-password" .= rootPassword' 
            <> "bpf-path" .= bPFPath' 
            <> "stap-path" .= sTapPath' 
            <> "submit-chunk-size" .= submitChunkSize'
            <> "platform-url" .= platformUrl'
            <> "secret" .= secret'
            <> "endpoint-uuid" .= uuid'
        )

{-# NOINLINE global' #-}
global' :: IORef LambdaGlobalInfo
global' = unsafePerformIO $ do
    info <- decodeFileThrow "config/lambda-settings.yml" :: IO LambdaGlobalInfo
    newIORef info

{-# NOINLINE global #-}
global :: LambdaGlobalInfo
global = unsafePerformIO $ readIORef global'
    


