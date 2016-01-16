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

-- Add operator op2 to parsed values if the condition is met
-- Otherwise, add op1 to operator stack
takeOperator :: ([String], OpList) -> String -> ([ String ], OpList)
takeOperator (rpn, []) op1 = (rpn, [op1])
takeOperator (rpn, op2:ops) op1 = if isOp op2 && (leftAssoc op1 && prec op1 <= prec op2 ||
                                      prec op1 < prec op2) then takeOperator (op2 : rpn, ops) op1
                                  else (rpn, op1:op2:ops)

-- Pop operators from operators' stack while left brace doesn't appear
-- If no left braces were found, we definitely have a brace mismatch
closingParen :: ([String], OpList) -> Maybe ([String], OpList)
closingParen (_, []) = Nothing
closingParen (rpn, "(":ops) = Just (rpn, ops)
closingParen (rpn, op:ops) = closingParen (op:rpn, ops)

-- Fold input string to reversed parsed list and to operators' stack
foldFunc :: ([String], OpList) -> String -> Maybe ([String], OpList)
foldFunc (rpn, ops) t
    | isOp t = Just $ takeOperator (rpn, ops) t
    | isNum t = Just (t:rpn, ops)
    | t == "(" = Just (rpn, t:ops)
    | t == ")" = closingParen (rpn, ops)
    | otherwise = Nothing

-- Fold operators' stack to parsed list
-- If left brace was found, we definitely have a brace mismatch
foldParse :: Maybe [String] -> String -> Maybe [String]
foldParse parse op = do
    guard $ op /= "("
    liftM (op:) parse

-- Join operator's stack to parsed list and return it as String
endParse :: Maybe ([String], OpList) -> Maybe RPN
endParse (Just (parse, ops)) = ( unwords . reverse) <$> foldl foldParse (Just parse) ops 
endParse _ = Nothing 

-- Parse infix form to RPN
parseToRPN :: String -> Maybe RPN
parseToRPN = endParse . foldM foldFunc ([], []) . words
