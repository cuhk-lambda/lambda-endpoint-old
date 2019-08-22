{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.Trace.Class where

import           Data.Aeson
import qualified Data.Text.Lazy   as T
import           GHC.Generics
import qualified Lambda.BPFTrace  as BPFT
import qualified Lambda.SystemTap as STAP
import           Model
import           Settings.Lambda

data TraceInfo =
  TraceInfo
    { process     :: !T.Text
    , functions   :: ![T.Text]
    , environment :: ![T.Text]
    , value       :: ![T.Text]
    , options     :: ![T.Text]
    , traceType   :: !T.Text
    }
  deriving (Generic, Show)

instance ToJSON TraceInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TraceInfo

class Trace a where
  generate :: a -> Int -> T.Text
  env :: a -> [(String, String)]
  opt :: a -> [String]
  getExec :: a -> String
  toInfo :: a -> TraceInfo
  fromInfo :: TraceInfo -> a

instance Trace STap where
  generate = STAP.generate
  env a =
    zipWith (\x y -> (T.unpack x, T.unpack y)) (sTapEnvironment a) (sTapValue a)
  opt = (map T.unpack) . sTapOptions
  getExec = const sTapPath
  toInfo (STap a b c d e) = TraceInfo a b c d e "STAP"
  fromInfo (TraceInfo a b c d e _) = STap a b c d e

instance Trace BPF where
  generate = BPFT.generate
  env a =
    zipWith (\x y -> (T.unpack x, T.unpack y)) (bPFEnvironment a) (bPFValue a)
  opt = (map T.unpack) . bPFOptions
  getExec = const bPFPath
  toInfo (BPF a b c d e) = TraceInfo a b c d e "BPF"
  fromInfo (TraceInfo a b c d e _) = BPF a b c d e
