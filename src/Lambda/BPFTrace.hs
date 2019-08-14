{-# LANGUAGE OverloadedStrings #-}

module Lambda.BPFTrace where

import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import           Model

bpfFromString :: String -> [String] -> [String] -> [String] -> [String] -> BPF -- for debug
bpfFromString a b c d e =
  BPF (T.pack a) (map T.pack b) (map T.pack c) (map T.pack d) (map T.pack e)

generate :: BPF -> Int -> T.Text
generate x t =
  TB.toLazyText $
  "BEGIN {printf(\"\\n\") ;}\n" <>
  mconcat (fmap (probeFunc $ bPFProcess x) (bPFFunctions x)) <>
  mconcat (fmap (probeRetFunc $ bPFProcess x) (bPFFunctions x)) <> timed t

timed :: Int -> TB.Builder
timed t = "interval:s:" <> TB.decimal t <> " { exit(); }\n"

probeFunc :: T.Text -> T.Text -> TB.Builder
probeFunc p f =
  "uprobe:" <>
  TB.fromLazyText p <>
  ":" <>
  TB.fromLazyText f <>
  " {\n\
            \   printf(\"{\\n    \\\"probe\\\": \\\"%s\\\",\\n\", probe);\n\
            \   printf(\"    \\\"uid\\\": %d,\\n    \\\"pid\\\": %d,\\n    \\\"tid\\\": %d,\\n    \\\"cpu\\\": %d,\\n\", uid, pid, tid, cpu);\n\
            \   printf(\"    \\\"time\\\": %lld,\\n    \\\"process\\\": \\\"%s\\\"\\n}\\n\", nsecs, comm);\n\
            \}\n"

probeRetFunc :: T.Text -> T.Text -> TB.Builder
probeRetFunc p f =
  "uretprobe:" <>
  TB.fromLazyText p <>
  ":" <>
  TB.fromLazyText f <>
  " {\n\
            \   printf(\"{\\n    \\\"return\\\": \\\"%s\\\",\\n\", probe);\n\
            \   printf(\"    \\\"uid\\\": %d,\\n    \\\"pid\\\": %d,\\n    \\\"tid\\\": %d,\\n    \\\"cpu\\\": %d,\\n\", uid, pid, tid, cpu);\n\
            \   printf(\"    \\\"retval\\\": \\\"%d\\\",\\n\", retval);\n\
            \   printf(\"    \\\"time\\\": %lld,\\n    \\\"process\\\": \\\"%s\\\"\\n}\\n\", nsecs, comm);\n\
            \}\n"
