module Lambda.Trace.Unsafe where

import qualified Data.HashTable.IO as H
import           System.IO.Unsafe
import           Lambda.Trace.Class
import           Control.Concurrent
import           System.Process
import           System.IO
type HashTable k v = H.CuckooHashTable k v
type TracePair = (ThreadId, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle), TraceInfo)
{-#NOINLINE runningTraces#-}
runningTraces :: HashTable String TracePair
runningTraces = unsafePerformIO H.new

putRunningTrace :: String -> TracePair -> IO ()
putRunningTrace path info = H.insert runningTraces path info

delRunningTrace :: String -> IO ()
delRunningTrace path = do
    res <- getRunningTrace path
    case res of
        Nothing -> return ()
        Just (_, p, _) -> do
            cleanupProcess p
    H.delete runningTraces path

getRunningTrace :: String -> IO (Maybe TracePair)
getRunningTrace path = H.lookup runningTraces path

getAllRunning :: IO [(String, TracePair)]
getAllRunning = H.toList runningTraces

killRunningTrace :: String -> IO ()
killRunningTrace path = do
    res <- getRunningTrace path
    case res of
        Nothing -> return ()
        Just (tid, _, _) -> killThread tid
    delRunningTrace path
    


