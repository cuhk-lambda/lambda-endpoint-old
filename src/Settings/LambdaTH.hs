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
    getSubmitChunkSize :: {-# UNPACK #-} !Int
} deriving Lift

instance FromJSON LambdaGlobalInfo where
    parseJSON = withObject "LambdaGlobalInfo" $ \v -> LambdaGlobalInfo
        <$> v .: "root-password"
        <*> v .: "bpf-path"
        <*> v .: "stap-path"
        <*> v .: "submit-chunk-size"

instance ToJSON LambdaGlobalInfo where
    -- this generates a Value
    toJSON (LambdaGlobalInfo rootPassword'  bPFPath' sTapPath' submitChunkSize') =
        object ["root-password" .= rootPassword', "bpf-path" .= bPFPath', "stap-path" .= sTapPath', "submit-chunk-size" .= submitChunkSize']
        
    -- this encodes directly to a bytestring Builder
    toEncoding (LambdaGlobalInfo rootPassword'  bPFPath' sTapPath' submitChunkSize') =
        pairs ("root-password" .= rootPassword' <> "bpf-path" .= bPFPath' <> "stap-path" .= sTapPath' <> "submit-chunk-size" .= submitChunkSize')


readConfig :: DecsQ
readConfig = do
    info <- decodeFileThrow "config/lambda-settings.yml" :: Q LambdaGlobalInfo
    [d|global :: LambdaGlobalInfo 
       global = info|]


