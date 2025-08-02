module IParser (IParser, IError (..), IResult (..), runIParser, anyChar, takeN, takeN2) where

import Control.DeepSeq (NFData (rnf))

data IParser i e r where
  IParseReturn :: r -> IParser i e r
  IParseFail :: e -> IParser i e r
  IParsePrimitive :: ([i] -> Either (IError e) (IResult i e r)) -> IParser i e r
  IParseApplicative :: IParser i e (r' -> r) -> IParser i e r' -> IParser i e r
  IParseBind :: IParser i e r' -> (r' -> IParser i e r) -> IParser i e r

instance Functor (IParser i e) where
  fmap f (IParseReturn r) = IParseReturn (f r)
  fmap _ (IParseFail e) = IParseFail e
  fmap f (IParsePrimitive p) = IParsePrimitive (fmap (fmap f) . p)
  fmap f (IParseApplicative lp rp) = IParseApplicative (fmap (f .) lp) rp
  fmap f (IParseBind pl fr) = IParseBind pl (fmap f . fr)

instance Applicative (IParser i e) where
  pure = IParseReturn
  (<*>) = IParseApplicative

instance Monad (IParser i e) where
  (>>=) = IParseBind

newtype IError e
  = IError'Application e

instance NFData e => NFData (IError e) where
  rnf (IError'Application err) = rnf err

data IResult i e r
  = IResult'Done r [i]
  | IResult'Partial (IParser i e r)

instance (NFData i, NFData r) => NFData (IResult i e r) where
  rnf (IResult'Done r is) = rnf r `seq` rnf is
  rnf (IResult'Partial _p) = ()

instance Functor (IResult i e) where
  fmap f (IResult'Done r is) = IResult'Done (f r) is
  fmap f (IResult'Partial p) = IResult'Partial (fmap f p)

runIParser :: IParser i e r -> [i] -> Either (IError e) (IResult i e r)
runIParser (IParseReturn r) is = Right (IResult'Done r is)
runIParser (IParseFail e) _is = Left (IError'Application e)
runIParser (IParsePrimitive f) is = f is
runIParser (IParseApplicative lp rp) is =
  do
    lResult <- runIParser lp is
    case lResult of
      IResult'Done lDone is' -> do
        rResult <- runIParser rp is'
        case rResult of
          IResult'Done rDone is'' -> return (IResult'Done (lDone rDone) is'')
          IResult'Partial rp' ->
            return
              ( IResult'Partial
                  ( IParseApplicative
                      (IParseReturn lDone)
                      rp'
                  )
              )
      IResult'Partial lp' -> return (IResult'Partial (IParseApplicative lp' rp))
runIParser (IParseBind pl f) is = do
  lResult <- runIParser pl is
  case lResult of
    IResult'Done lDone is' -> runIParser (f lDone) is'
    IResult'Partial pl' -> return (IResult'Partial (IParseBind pl' f))

anyChar :: IParser Char e Char
anyChar = IParsePrimitive $ \case
  [] -> Right (IResult'Partial anyChar)
  (x : xs) -> Right (IResult'Done x xs)

takeN :: Int -> IParser Char e String
takeN n = IParsePrimitive $ \is ->
  let (consumed, is') = splitAt n is
      nConsumed = length consumed
   in if nConsumed < n
        then
          Right
            ( IResult'Partial
                ( IParseApplicative
                    (IParseReturn (++ consumed))
                    (takeN (n - nConsumed))
                )
            )
        else Right (IResult'Done consumed is')

takeN2 :: Int -> IParser Char e String
takeN2 n = IParsePrimitive $ \is ->
  let (consumed, is') = splitAt n is
      nConsumed = length consumed
   in if nConsumed < n
        then
          Right
            ( IResult'Partial
                ( (++ consumed) <$> takeN2 (n - nConsumed)
                )
            )
        else Right (IResult'Done consumed is')
