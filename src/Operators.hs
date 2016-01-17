module Operators
(isOp, leftAssoc, prec,
isNum, isDot, isBrace)
where
import Text.Read(readMaybe)

operators = "+-*/^"

isOp :: String -> Bool
isOp [t] = t `elem` operators
isOp _ = False

isNum :: String -> Bool
isNum str = case readMaybe str :: Maybe Double of 
    Just _ -> True
    Nothing -> False

isDot d = d == '.'
isBrace d = d `elem` "()"

leftAssoc :: String -> Bool
leftAssoc "^" = False
leftAssoc _   = True 

prec "+" = 2
prec "-" = 2
prec "*" = 3
prec "/" = 3
prec "^" = 4
