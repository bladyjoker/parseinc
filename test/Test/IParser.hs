module Test.IParser (spec) where

import IParser (
  IResult (IResult'Done, IResult'Partial),
  anyChar,
  runIParser,
  takeN,
  takeN2,
 )
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
 )
import Test.QuickCheck (Testable (property), (==>))

spec :: Spec
spec = do
  describe "IParser" $ do
    describe "takeN" $ do
      it "takes exactly n characters when available" $ do
        let result = runIParser (takeN 3) "hello"
        case result of
          Right (IResult'Done taken remaining) -> do
            taken `shouldBe` "hel"
            remaining `shouldBe` "lo"
          _ -> expectationFailure "Expected Done result"

      it "returns partial when not enough input" $ do
        let result = runIParser (takeN 5) "hi"
        case result of
          Right (IResult'Partial _) -> return ()
          _ -> expectationFailure "Expected Partial result"

      it "handles empty input" $ do
        let result = runIParser (takeN 3) ""
        case result of
          Right (IResult'Partial _) -> return ()
          _ -> expectationFailure "Expected Partial result"

    describe "takeN2" $ do
      it "takes exactly n characters when available" $ do
        let result = runIParser (takeN2 3) "hello"
        case result of
          Right (IResult'Done taken remaining) -> do
            taken `shouldBe` "hel"
            remaining `shouldBe` "lo"
          _ -> expectationFailure "Expected Done result"

      it "returns partial when not enough input" $ do
        let result = runIParser (takeN2 5) "hi"
        case result of
          Right (IResult'Partial _) -> return ()
          _ -> expectationFailure "Expected Partial result"

    describe "anyChar" $ do
      it "consumes one character" $ do
        let result = runIParser anyChar "abc"
        case result of
          Right (IResult'Done c remaining) -> do
            c `shouldBe` 'a'
            remaining `shouldBe` "bc"
          _ -> expectationFailure "Expected Done result"

      it "returns partial on empty input" $ do
        let result = runIParser anyChar ""
        case result of
          Right (IResult'Partial _) -> return ()
          _ -> expectationFailure "Expected Partial result"

  describe "Property tests" $ do
    it "takeN n should take at most n characters" $ property $ \n input ->
      n
        >= 0
        ==> case runIParser (takeN n) input of
          Right (IResult'Done taken _) -> length taken == n
          Right (IResult'Partial _) -> length input < n
          Left _ -> False

    it "takeN2 should behave like takeN for complete input" $ property $ \n input ->
      let takeNResult = runIParser (takeN n) input
          takeN2Result = runIParser (takeN2 n) input
       in n >= 0
            && length input
              >= n
            ==> case (takeNResult, takeN2Result) of
              (Right (IResult'Done taken1 remaining1), Right (IResult'Done taken2 remaining2)) -> taken1 == taken2 && remaining1 == remaining2
              _ -> False
