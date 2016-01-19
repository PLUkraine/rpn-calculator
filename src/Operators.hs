module Operators
(isOp, leftAssoc, prec, isUnary,
isNum, isDot, isBrace, isFunc, 
isFuncSepar, isConst, getConst,
asUnary, separ)
where
import Text.Read(readMaybe)
import Data.Maybe(isJust, fromJust)
import Data.List(lookup)

-- constant tokens
operators = "+-*/^np"
unaries   = [("-","n"), ("+", "p")]
functions = ["sin", "cos", "max"]
constants = [("pi", pi), ("e", exp 1)]
separ = ","

-- |Get unary representation of operator
-- If operator has no unary representation, exception is thrown
asUnary t = fromJust $ lookup t unaries

-- |Check if token is unary opearator
isUnary :: String -> Bool
isUnary t = isJust $ lookup t unaries

-- |Check if token is mathematical constant
isConst :: String -> Bool
isConst t = isJust $ lookup t constants  

-- |Get value of the constant
-- If symbol isn't constant, exception is thrown
-- Token is constant if 'isConst' evaluates to True
getConst :: String -> Double
getConst t = fromJust $ lookup t constants

-- |Check if token is function
isFunc :: String -> Bool
isFunc t = t `elem` functions

-- |Check if token is operator
isOp :: String -> Bool
isOp [t] = t `elem` operators
isOp _ = False

-- |Check if token is floating point number
isNum :: String -> Bool
isNum str = isJust (readMaybe str :: Maybe Double)

-- |Check if symbol is dot
isDot d = d == '.'
-- |Check if symbol is brace (either left or right)
isBrace d = d `elem` "()"

-- |Check if symbol is function argument separator
isFuncSepar d = d == separ

-- |Check if symbol is left associative operator
leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc "n" = False
leftAssoc "p" = False
leftAssoc _   = True 

-- |Get precendence of operator.
-- If argument isn't operator, exception is thrown
prec "+" = 2
prec "-" = 2
prec "*" = 3
prec "/" = 3
prec "^" = 4
prec "n" = 4
prec "p" = 4
