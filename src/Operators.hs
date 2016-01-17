module Operators
(isOp, leftAssoc, prec, isUnary,
isNum, isDot, isBrace, isFunc, 
isFuncSepar, isConst, getConst,
asUnary, separ)
where
import Text.Read(readMaybe)
import Data.Maybe(isJust, fromJust)
import Data.List(lookup)

operators = "+-*/^np"
unaries   = [("-","n"), ("+", "p")]
functions = ["sin", "cos", "max"]
constants = [("pi", pi), ("e", exp 1)]
separ = ","

asUnary t = fromJust $ lookup t unaries

isUnary :: String -> Bool
isUnary t = isJust $ lookup t unaries

isConst :: String -> Bool
isConst t = isJust $ lookup t constants  

getConst :: String -> Double
getConst t = fromJust $ lookup t constants

isFunc :: String -> Bool
isFunc t = t `elem` functions

isOp :: String -> Bool
isOp [t] = t `elem` operators
isOp _ = False

isNum :: String -> Bool
isNum str = isJust (readMaybe str :: Maybe Double)

isDot d = d == '.'
isBrace d = d `elem` "()"

isFuncSepar d = d == separ

leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc "n" = False
leftAssoc "p" = False
leftAssoc _   = True 

prec "+" = 2
prec "-" = 2
prec "*" = 3
prec "/" = 3
prec "^" = 4
prec "n" = 4
prec "p" = 4
