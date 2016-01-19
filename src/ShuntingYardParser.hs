{-
 - Visit Wikipedia for full algorithm
 -}
module ShuntingYardParser
(parseToRPN)
where
import Operators
import Lexer
import Data.List(intercalate)
import Control.Monad
import Control.Applicative((<$>), (<*>))

type RPN = String
type Token = String
type OpList = [String]

-- |Check if token is unary operator
-- Operator is unary if in infix form last token seen 
-- is operand (operator, function, left brace or function argument separator)
isUnaryInContext :: Token -> Token -> Bool
isUnaryInContext lastToken curToken = isUnary curToken && or ([isOp, (==) "(", isFunc, isFuncSepar] <*> [lastToken])

-- |Function for dealing with operators
-- Add operator op2 to parsed values if the condition is met
-- Otherwise, add op1 to operator stack
takeOperator :: ([Token], OpList) -> Token -> ([ Token ], OpList, Token)
takeOperator (rpn, []) op1 = (rpn, [op1], op1)
takeOperator (rpn, op2:ops) op1 = if isOp op2 && (leftAssoc op1 && prec op1 <= prec op2 ||
                                      prec op1 < prec op2)
                                  then takeOperator (op2 : rpn, ops) op1
                                  else (rpn, op1:op2:ops, op1)
--
-- |Function for dealing with closing brace
-- Pop operators from operators' stack while left brace doesn't appear
-- If left brace is encountered and operator is on top of the stack after it,
-- pop operator and add it to rpn tokens list
-- If no left braces were found, we definitely have a brace mismatch
closingParen :: ([Token], OpList) -> Either Token ([Token], OpList, Token)
-- if no more operators are available on the stack, right bracket is mismatched
closingParen (_, []) = Left "Right brace mismatched" 
closingParen (rpn, "(":op:ops)
-- trying to take function token from the stack
    | isFunc op = Right (op:rpn, ops, ")")
-- otherwise, just take the left brace from the stack
    | otherwise = Right (rpn, op:ops, ")")
closingParen (rpn, "(":ops) = Right (rpn, ops, ")")
closingParen (rpn, op:ops) = closingParen (op:rpn, ops)

-- |Puts operators to result list untill first left brace
-- If no left brace was found, than we've got either bracket mismatch or wild separator
putFunctionArgument :: ([Token], OpList) -> Either String ([Token], OpList, Token)
putFunctionArgument (_, []) = Left "Left brace mismatch or wild separator"
putFunctionArgument (rpn, "(":ops) = Right (rpn, "(":ops, ",") 
putFunctionArgument (rpn, op:ops) = putFunctionArgument (op:rpn, ops)

-- |Fold input string to reversed parsed list and to operators' stack
foldFunc :: ([Token], OpList, Token) -> String -> Either String ([Token], OpList, Token)
foldFunc (rpn, ops, prev) t
    | isOp t = Right $ takeOperator (rpn, ops) (if isUnaryInContext prev t then asUnary t else t)
    | isFunc t = Right (rpn, t:ops, t)
    | isFuncSepar t = putFunctionArgument (rpn, ops)
    | isNum t || isConst t = Right (t:rpn, ops, t)
    | t == "(" = Right (rpn, t:ops, t)
    | t == ")" = closingParen (rpn, ops)
    | otherwise = Left $ "Unrecognized token \"" ++ t ++ "\""

-- |Fold operators' stack to parsed list
-- If left brace was found, we definitely have a brace mismatch
foldParse :: [Token] -> String -> Either String [Token]
foldParse parse op = if op/="("
                     then Right $ op:parse
                     else Left "Mismatched left brace"

-- |Join operator's stack to parsed list and return it as String
endParse :: ([Token], OpList, Token) -> Either String RPN
endParse (parse, ops, _) = (unwords . reverse) <$> foldM foldParse parse ops 

-- |Parse infix form to RPN
parseToRPN :: String -> Either String RPN
parseToRPN = endParse <=< foldM foldFunc ([], [], "(" ) <=< parseToTokens 
