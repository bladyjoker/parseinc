module Main (main) where

import Benchmark.IParser qualified
import Control.DeepSeq (NFData, deepseq, force)
import Criterion.Main

main :: IO ()
main =
  defaultMain Benchmark.IParser.spec
