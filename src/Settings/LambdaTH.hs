{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
module Settings.LambdaTH where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Aeson
import Data.Yaml
data LambdaGlobalInfo = LambdaGlobalInfo {
    getRootPassword :: {-# UNPACK #-} !String,
    getBPFPath :: {-# UNPACK #-} !String,
    getSTapPath :: {-# UNPACK #-} !String,
    getSubmitChunkSize :: {-# UNPACK #-} !Int,
    getPlatformUrl :: {-# UNPACK #-} !String
} deriving Lift

instance FromJSON LambdaGlobalInfo where
    parseJSON = withObject "LambdaGlobalInfo" $ \v -> LambdaGlobalInfo
        <$> v .: "root-password"
        <*> v .: "bpf-path"
        <*> v .: "stap-path"
        <*> v .: "submit-chunk-size"
        <*> v .: "platform-url"

instance ToJSON LambdaGlobalInfo where
    -- this generates a Value
    toJSON (LambdaGlobalInfo rootPassword'  bPFPath' sTapPath' submitChunkSize' platformUrl') =
        object [
            "root-password" .= rootPassword', 
            "bpf-path" .= bPFPath', 
            "stap-path" .= sTapPath', 
            "submit-chunk-size" .= submitChunkSize',
            "platform-url" .= platformUrl'
        ]
        
    -- this encodes directly to a bytestring Builder
    toEncoding (LambdaGlobalInfo rootPassword'  bPFPath' sTapPath' submitChunkSize' platformUrl') =
        pairs (
            "root-password" .= rootPassword' 
            <> "bpf-path" .= bPFPath' 
            <> "stap-path" .= sTapPath' 
            <> "submit-chunk-size" .= submitChunkSize'
            <> "platform-url" .= platformUrl'
        )


readConfig :: DecsQ
readConfig = do
    info <- decodeFileThrow "config/lambda-settings.yml" :: Q LambdaGlobalInfo
    [d|global :: LambdaGlobalInfo 
       global = info|]


