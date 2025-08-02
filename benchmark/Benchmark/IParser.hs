module Benchmark.IParser (spec) where

import Control.DeepSeq (NFData, deepseq, force)
import Criterion.Main
import IParser (
  IError,
  IResult (IResult'Done, IResult'Partial),
  runIParser,
  takeN,
  takeN2,
 )

-- Generate test strings of various sizes
smallString :: String
smallString = replicate 100 'a'

mediumString :: String
mediumString = replicate 1000 'a'

largeString :: String
largeString = replicate 10000 'a'

hugeString :: String
hugeString = replicate 100000 'a'

-- Force evaluation of parser results to ensure fair benchmarking
forceResult :: NFData i => (NFData r, NFData e) => IResult i e r -> IResult i e r
forceResult result = result `deepseq` result

-- Benchmark takeN vs takeN2 with different input sizes and chunk sizes
benchTakeN :: String -> String -> Int -> Benchmark
benchTakeN name input n =
  bgroup
    name
    [ bench ("takeN " ++ show n) $ nf (fmap forceResult <$> runIParser (takeN @String n)) input
    , bench ("takeN2 " ++ show n) $ nf (fmap forceResult <$> runIParser (takeN2 @String n)) input
    ]

-- Benchmark multiple consecutive takes (simulating real-world parsing)
benchConsecutiveTakes :: String -> String -> [Int] -> Benchmark
benchConsecutiveTakes name input sizes =
  bgroup
    name
    [ bench "takeN consecutive" $ nf (benchTakeNSequence sizes) input
    , bench "takeN2 consecutive" $ nf (benchTakeN2Sequence sizes) input
    ]
  where
    benchTakeNSequence :: [Int] -> String -> Either (IError String) (IResult Char String String)
    benchTakeNSequence [] _ = Right (IResult'Done "" "")
    benchTakeNSequence (n : ns) inp = case runIParser (takeN n) inp of
      Right (IResult'Done _ remaining) -> benchTakeNSequence ns remaining
      result -> result

    benchTakeN2Sequence :: [Int] -> String -> Either (IError String) (IResult Char String String)
    benchTakeN2Sequence [] _ = Right (IResult'Done "" "")
    benchTakeN2Sequence (n : ns) inp = case runIParser (takeN2 n) inp of
      Right (IResult'Done _ remaining) -> benchTakeN2Sequence ns remaining
      result -> result

spec =
  [ bgroup
      "Small chunks (10-50 chars)"
      [ benchTakeN "100 char string" smallString 10
      , benchTakeN "100 char string" smallString 25
      , benchTakeN "100 char string" smallString 50
      , benchTakeN "1000 char string" mediumString 10
      , benchTakeN "1000 char string" mediumString 25
      , benchTakeN "1000 char string" mediumString 50
      ]
  , bgroup
      "Medium chunks (100-500 chars)"
      [ benchTakeN "1000 char string" mediumString 100
      , benchTakeN "1000 char string" mediumString 250
      , benchTakeN "1000 char string" mediumString 500
      , benchTakeN "10000 char string" largeString 100
      , benchTakeN "10000 char string" largeString 250
      , benchTakeN "10000 char string" largeString 500
      ]
  , bgroup
      "Large chunks (1000-5000 chars)"
      [ benchTakeN "10000 char string" largeString 1000
      , benchTakeN "10000 char string" largeString 2500
      , benchTakeN "10000 char string" largeString 5000
      , benchTakeN "100000 char string" hugeString 1000
      , benchTakeN "100000 char string" hugeString 2500
      , benchTakeN "100000 char string" hugeString 5000
      ]
  , bgroup
      "Consecutive parsing scenarios"
      [ benchConsecutiveTakes "Parse 10x10 from 1000 chars" mediumString (replicate 10 10)
      , benchConsecutiveTakes "Parse 5x100 from 1000 chars" mediumString (replicate 5 100)
      , benchConsecutiveTakes "Parse 100x10 from 10000 chars" largeString (replicate 100 10)
      , benchConsecutiveTakes "Parse 20x50 from 10000 chars" largeString (replicate 20 50)
      ]
  , bgroup
      "Edge cases"
      [ benchTakeN "Take more than available" smallString 200
      , benchTakeN "Take exact length" smallString 100
      , benchTakeN "Take zero chars" smallString 0
      ]
  ]
