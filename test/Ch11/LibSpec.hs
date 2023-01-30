module Ch11.LibSpec (spec) where

import Ch11.Lib
  ( Assignment,
    Expr (..),
    Identity (..),
    Op (..),
    eval,
    eval',
    fromIdentity,
    parseHex,
    toIdentity,
  )
import qualified Control.DeepSeq as DS
import Control.Exception (ArithException (..))
import qualified Control.Exception as Ex
import qualified Control.Monad as M
import qualified Control.Monad.Trans.Maybe as TMb
import qualified Control.Monad.Trans.State as TS
import qualified Data.Either as E
import qualified Data.Maybe as Mb
import Data.Set (Set)
import qualified Data.Set as S
import qualified Language.Haskell.Interpreter as I
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "evaluates expression" $ do
    -- modifyMaxSuccess (const 50) $
    prop "eval" $
      forAll genTestData $
        \(expr, a) -> ioProperty $ do
          verify (eval expr a) expr a

    -- modifyMaxSuccess (const 50) $
    prop "eval'" $
      forAll genTestData $
        \(expr, a) -> ioProperty $ do
          let actual = TS.evalState (TMb.runMaybeT (eval' expr)) a
          verify actual expr a

  {-
  Following tests use `generate` to work in the context of IO, and the
  tests run only once.
  If we worked in the context of the generators instead, the tests
  would run 100 times.
  -}
  describe "fails to evaluate expression if unknown variable" $ do
    prop "eval" $
      ioProperty $ do
        n <- generate $ chooseInt (1, 10)
        expr <- generate $ genExprWithVars n
        return (Mb.isNothing (eval expr []))

    prop "eval'" $
      ioProperty $ do
        n <- generate $ chooseInt (1, 10)
        expr <- generate $ genExprWithVars n
        return (Mb.isNothing (TS.evalState (TMb.runMaybeT (eval' expr)) []))

  describe "T and Identity T" $ do
    prop "are isomorphic for any type T" $
      \x -> do
        (fromIdentity . toIdentity) x `shouldBe` (x :: Int)
        (toIdentity . fromIdentity) (I x) `shouldBe` I x

  describe "hex parser" $ do
    prop "successfully parses hex string" $
      forAll genHex $
        \xs -> parseHex ("0x" ++ xs) == xs
    prop "fails to parse invalid hex string" $
      forAll genHex (null . parseHex)
    prop "fails to parse empty string" $
      null $
        parseHex ""

verify :: Maybe Integer -> Expr -> Assignment -> IO Bool
verify actual expr a = do
  let x = show expr
  expected <- evalExpr x a
  -- putStrLn $ "expr: " ++ x ++ ", assignment: " ++ show a
  -- putStrLn $ "expected: " ++ show expected
  return (actual == expected)

evalExpr :: String -> Assignment -> IO (Maybe Integer)
evalExpr expr a = Ex.handle handler $ do
  i <- I.runInterpreter $ do
    I.setImports ["Prelude"]
    -- `var <- return val` also works
    let stmts = map (\(var, val) -> "let " ++ var ++ " = " ++ show val) a
    M.forM_ stmts $ \s -> do
      I.runStmt s

    I.interpret expr (I.as :: Integer)

  -- without force, exception is not caught
  (Ex.evaluate . DS.force) (E.either (const Nothing) Just i)
  where
    handler :: ArithException -> IO (Maybe Integer)
    handler DivideByZero = return Nothing
    handler ex = error $ show ex

genInteger :: Gen Integer
genInteger = chooseInteger (0, 500)

genLit :: Gen Expr
genLit = Literal <$> genInteger

genVar :: Gen Expr
genVar = do
  c <- elements ['a' .. 'z']
  return (Var [c])

genAlphaNum :: Gen Expr
genAlphaNum = oneof [genLit, genVar]

genOp :: Gen Op
genOp = elements [Add, Subtract, Multiply, Divide]

genExpr :: Int -> Gen Expr
genExpr n =
  if n == 1
    then genAlphaNum
    else do
      m <- chooseInt (1, n - 1)
      (Op <$> genOp) <*> genExpr m <*> genExpr (n - m)

genExprWithVars :: Int -> Gen Expr
genExprWithVars n = suchThat (genExpr n) (not . null . getVars)

getVars :: Expr -> Set String
getVars (Literal _) = S.empty
getVars (Var x) = S.singleton x
getVars (Op _ l r) = S.union (getVars l) (getVars r)

genTestData :: Gen (Expr, Assignment)
genTestData = do
  -- n literals + variables
  n <- chooseInt (1, 10)
  expr <- genExpr n
  let vars = getVars expr
  values <- vectorOf (S.size vars) genInteger
  let assignment = zip (S.toList vars) values
  return (expr, assignment)

genHex :: Gen String
genHex = do
  n <- chooseInt (0, 10)
  vectorOf n $ elements "abcdefABCDEF0123456789"
