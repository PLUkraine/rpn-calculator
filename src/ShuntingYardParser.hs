{-
 - Visit Wikipedia for full algorithm
 -}
module ShuntingYardParser
(parseToRPN)
where
import Operators
import Data.List(intercalate)
import Control.Monad
import Control.Applicative((<$>))

type RPN = String
type OpList = [String]

-- Function for dealing with operators
-- Add operator op2 to parsed values if the condition is met
-- Otherwise, add op1 to operator stack
takeOperator :: ([String], OpList) -> String -> ([ String ], OpList)
takeOperator (rpn, []) op1 = (rpn, [op1])
takeOperator (rpn, op2:ops) op1 = if isOp op2 && (leftAssoc op1 && prec op1 <= prec op2 ||
                                      prec op1 < prec op2) then takeOperator (op2 : rpn, ops) op1
                                  else (rpn, op1:op2:ops)

-- Function for dealing with closing brace
-- Pop operators from operators' stack while left brace doesn't appear
-- If no left braces were found, we definitely have a brace mismatch
closingParen :: ([String], OpList) -> Either String ([String], OpList)
closingParen (_, []) = Left "Right brace mismatched" 
closingParen (rpn, "(":ops) = Right (rpn, ops)
closingParen (rpn, op:ops) = closingParen (op:rpn, ops)

-- Fold input string to reversed parsed list and to operators' stack
foldFunc :: ([String], OpList) -> String -> Either String ([String], OpList)
foldFunc (rpn, ops) t
    | isOp t = Right $ takeOperator (rpn, ops) t
    | isNum t = Right (t:rpn, ops)
    | t == "(" = Right (rpn, t:ops)
    | t == ")" = closingParen (rpn, ops)
    | otherwise = Left $ "Parse error on \"" ++ t ++ "\""

-- Fold operators' stack to parsed list
-- If left brace was found, we definitely have a brace mismatch
foldParse :: [String] -> String -> Either String [String]
foldParse parse op = if op/="("
                     then Right $ op:parse
                     else Left "Mismatched left brace"

-- Join operator's stack to parsed list and return it as String
endParse :: ([String], OpList) -> Either String RPN
endParse (parse, ops) = (unwords . reverse) <$> foldM foldParse parse ops 

-- Parse infix form to RPN
parseToRPN :: String -> Either String RPN
parseToRPN = endParse <=< foldM foldFunc ([], []) . words
