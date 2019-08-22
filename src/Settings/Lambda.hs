{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Settings.Lambda where

import qualified Data.ByteString.Char8  as BS
import           Settings.Lambda.Unsafe

rootPassword :: String
rootPassword = getRootPassword global

bPFPath :: String
bPFPath = getBPFPath global

sTapPath :: String
sTapPath = getSTapPath global

submitChunkSize :: Int
submitChunkSize = getSubmitChunkSize global

platformUrl :: String
platformUrl = getPlatformUrl global

secret :: BS.ByteString
secret = BS.pack $ getSecret global

endpointUUID :: String
endpointUUID = getUUID global
