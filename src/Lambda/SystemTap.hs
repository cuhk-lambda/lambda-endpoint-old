{-# LANGUAGE OverloadedStrings #-}

module Lambda.SystemTap where

import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import           Model

stapFromString :: String -> [String] -> [String] -> [String] -> [String] -> STap -- for debug
stapFromString a b c d e =
  STap (T.pack a) (map T.pack b) (map T.pack c) (map T.pack d) (map T.pack e)

generate :: STap -> Int -> T.Text
generate x t =
  TB.toLazyText $
  "probe begin { print(\"\\n\"); }\n" <>
  mconcat (fmap (probeFunc $ sTapProcess x) (sTapFunctions x)) <>
  mconcat (fmap (probeRetFunc $ sTapProcess x) (sTapFunctions x)) <> timed t

timed :: Int -> TB.Builder
timed t = "probe timer.s(" <> TB.decimal t <> ") {exit(); }\n"

probeFunc :: T.Text -> T.Text -> TB.Builder
probeFunc p f =
  "probe " <>
  "process(\"" <>
  TB.fromLazyText p <>
  "\").function(\"" <>
  TB.fromLazyText f <>
  "\").call {\n\
    \   printf(\"{\\n    \\\"probe\\\": \\\"%s\\\",\\n\", ppfunc())\n\
    \   printf(\"    \\\"args\\\": \\\"%s\\\",\\n\", $$parms)\n\
    \   printf(\"    \\\"uid\\\": %d,\\n    \\\"pid\\\": %d,\\n    \\\"tid\\\": %d,\\n    \\\"cpu\\\": %d,\\n\", uid(), pid(), tid(), cpu())\n\
    \   printf(\"    \\\"time\\\": %ld,\\n    \\\"process\\\": \\\"%s\\\"\\n}\\n\", gettimeofday_us(), execname())\n\
    \}\n"

probeRetFunc :: T.Text -> T.Text -> TB.Builder
probeRetFunc p f =
  "probe " <>
  "process(\"" <>
  TB.fromLazyText p <>
  "\").function(\"" <>
  TB.fromLazyText f <>
  "\").return {\n\
    \   printf(\"{\\n    \\\"return\\\": \\\"%s\\\",\\n\", ppfunc())\n\
    \   printf(\"    \\\"uid\\\": %d,\\n    \\\"pid\\\": %d,\\n    \\\"tid\\\": %d,\\n    \\\"cpu\\\": %d,\\n\", uid(), pid(), tid(), cpu())\n\
    \   printf(\"    \\\"retval\\\": \\\"%s\\\",\\n\", $$return)\n\
    \   printf(\"    \\\"time\\\": %ld,\\n    \\\"process\\\": \\\"%s\\\"\\n}\\n\", gettimeofday_us(), execname())\n\
    \}\n"
