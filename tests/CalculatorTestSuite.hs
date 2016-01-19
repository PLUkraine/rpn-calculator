module Main (
    main
 ) where
 
import qualified Test.Framework as T
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx
import Calculator
import ShuntingYardParser
import Data.Either(isLeft)
import Control.Monad((>=>))

main :: IO ()
main = T.defaultMain tests

tests :: [T.Test]
tests =
  [
    T.testGroup "Testing errors" $ hUnitTestToTests $ TestList 
    [
        TestLabel "Right brace mismatch" testRightBraceMismatch,
        TestLabel "Left brace mismatch" testLeftBraceMismatch,
        TestLabel "Stray separator" testStraySeparator, 
        TestLabel "Stray dot" testStrayDot, 
        TestLabel "Bad double1" testBadDouble1, 
        TestLabel "Bad double2" testBadDouble2,
        TestLabel "Bad double3" testBadDouble3,
        TestLabel "Bad double4" testBadDouble4
    ],
    T.testGroup "Testing operators" $ hUnitTestToTests $ TestList 
    [
        TestLabel "Unary minus" testUnaryMinus,
        TestLabel "Unary Plus" testUnaryPlus,
        TestLabel "Right associativity of pow" testRightAssociativityOfPow,
        TestLabel "Precedence" testPrecedence,
        TestLabel "Unary plus in expression" testUnaryPlusInExpression
    ],
    T.testGroup "Testing constants" $ hUnitTestToTests $ TestList
    [
        TestLabel "Zero" testZeroConstant,
        TestLabel "Pi" testPi,
        TestLabel "Euler number" testE
    ],
    T.testGroup "Testing examples from GtHub" $ hUnitTestToTests $ TestList
    [
        TestLabel "Example1" testExample1,
        TestLabel "Example2" testExample2,
        TestLabel "Example3" testExample3,
        TestLabel "Example4" testExample4
    ]
  ]


-- Constants' tests
testZeroConstant = 0 `approx` evalToRight "    0.0   "
testPi = pi `approx` evalToRight "  pi   "
testE = exp 1 `approx` evalToRight "  e   "

-- Operators' test
testUnaryMinus = -133 `approx` evalToRight "-   --133.0"
testUnaryPlus = 113 `approx` evalToRight "+-113.0"
testRightAssociativityOfPow = 2 `approx` evalToRight "2^1^3"
testPrecedence = -17 `approx` evalToRight "1+2*-3^2"
testUnaryPlusInExpression = 19 `approx` evalToRight "1+2*+-3^2"

-- Examples' test
testExample1 = 163 `approx` evalToRight "1+2*3^4"
testExample2 = (sin (pi/2)*cos 0) `approx` evalToRight "sin(pi/2) * cos(0)"
testExample3 = 27 `approx` evalToRight "max(2.2, max(4^2, 3^3))"
testExample4 = 0.754416255 `approx` evalToRight "13^(3^(1/2)+(-2))*3/2"

-- Error's test
testRightBraceMismatch = testCaseError "max(3, max(3,4)"
testLeftBraceMismatch = testCaseError "1 + (3 + 2^3 ) )"
testStraySeparator = testCaseError "1 , (3 + 2^3 ) "
testStrayDot = testCaseError "1 + . 3 + 4 "
testBadDouble1 = testCaseError "1. 3 + 4 "
testBadDouble2 = testCaseError "1 .3 + 4 "
testBadDouble3 = testCaseError "1 . 3 + 4 "
testBadDouble4 = testCaseError "1. + 4 "

-- Auxiliary functions

-- Default precision for tests
eps = 1e-9
-- infix operator for convenience
approx :: Double -> Double -> Test
infixl 1 `approx`
approx a x = TestCase $ assertApproxEqual "" eps a x
-- expression evaluator
eval = parseToRPN >=> runCalculator

getRight :: Either a b -> b
getRight (Right a) = a
getRight (Left a) = error "No right value"

evalToRight :: String -> Double
evalToRight str = getRight $ eval str

testCaseError :: String -> Test 
testCaseError str = TestCase $ assertBool "" $ isLeft $ eval str 
