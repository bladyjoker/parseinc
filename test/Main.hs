module Main (main) where

import Test.Hspec.Runner (
  defaultConfig,
  hspecWith,
 )
import Test.IParser qualified

main :: IO ()
main = hspecWith defaultConfig Test.IParser.spec
