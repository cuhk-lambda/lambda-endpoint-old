module Lambda.Trace.Unsafe where

import qualified Data.HashTable.IO as H
import           System.IO.Unsafe
import           Lambda.Trace.Class
type HashTable k v = H.CuckooHashTable k v

{-#NOINLINE runningTraces#-}
runningTraces :: HashTable String TraceInfo
runningTraces = unsafePerformIO H.new

putRunningTrace :: String -> TraceInfo -> IO ()
putRunningTrace path info = H.insert runningTraces path info

delRunningTrace :: String -> IO ()
delRunningTrace path = H.delete runningTraces path

getRunningTrace :: String -> IO (Maybe TraceInfo)
getRunningTrace path = H.lookup runningTraces path

getAllRunning :: IO [(String, TraceInfo)]
getAllRunning = H.toList runningTraces

