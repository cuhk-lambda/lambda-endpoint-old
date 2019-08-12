{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Settings.Lambda where
import Settings.LambdaTH

readConfig
rootPassword :: String
rootPassword = getRootPassword global

bPFPath :: String
bPFPath = getBPFPath global

sTapPath :: String
sTapPath = getSTapPath global

submitChunkSize :: Int
submitChunkSize = getSubmitChunkSize global
