module Lexer
(parseToTokens)
where

import Operators
import Data.Char(isDigit, isSpace, isAlpha, isAlphaNum)
import Control.Applicative((<$>))
import Control.Monad(foldM)

data State = Number | Dot | NoDotNumber | Function | Whitespace
type Token = String

-- Appends parse to tokens' list if it isn't empty
appendToken :: String -> [String] -> [String]
appendToken parse tokens = if null parse 
                           then tokens 
                           else parse:tokens

-- Add input symbol to parse
consumeToParse :: ([ Token ], Token, State, Int) -> Char -> State -> ([ Token ], Token, State, Int)
consumeToParse (tokens, parse, _, pos) t newState = (tokens, parse ++ [t], newState, pos+1)

-- Push parse to tokens and ignore input token
pushParseIgnoringInput :: ([ Token ], Token, State, Int) -> ([ Token ], Token, State, Int)
pushParseIgnoringInput (tokens, parse, _, pos) = (appendToken parse tokens, "", Whitespace, pos+1)

-- Push input sympol and then parse to tokens
pushParseWithInput :: ([ Token ], Token, State, Int) -> Char -> ([ Token ], Token, State, Int)
pushParseWithInput (tokens, parse, _, pos) t = ([t]:appendToken parse tokens, "", Whitespace, pos+1)
                                      

-- Function that acts like automaton
foldToTokens :: ([ Token ], Token, State, Int) -> Char -> Either String ([Token], Token, State, Int)
foldToTokens arg@(tokens, parse, Dot, pos) t
    | isDigit t = Right $ consumeToParse arg t NoDotNumber
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos
foldToTokens arg@(tokens, parse, Number, pos) t
    | isDigit t = Right $ consumeToParse arg t Number
    | isDot   t = Right $ consumeToParse arg t Dot
    | isSpace t = Right $ pushParseIgnoringInput arg
    | isOp [t] || isBrace t || isFuncSepar [t] = Right $ pushParseWithInput arg t
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos
foldToTokens arg@(tokens, parse, NoDotNumber, pos) t
    | isDigit t = Right $ consumeToParse arg t NoDotNumber
    | isSpace t = Right $ pushParseIgnoringInput arg
    | isOp  [t] || isBrace t || isFuncSepar [t] = Right $ pushParseWithInput arg t
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos
foldToTokens arg@(tokens, parse, Function, pos) t
    | isAlphaNum t = Right $ consumeToParse arg t Function
    | isSpace t = Right $ pushParseIgnoringInput arg
    | isOp [t] || isBrace t || isFuncSepar [t] = Right $ pushParseWithInput arg t
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos
foldToTokens arg@(tokens, _, Whitespace, pos) t
    | isDigit t = Right (tokens, [t], Number, pos+1)
    | isAlpha t = Right (tokens, [t], Function, pos+1)
    | isBrace t || isFuncSepar [ t ] || isOp [ t ] = Right ([t]:tokens, "", Whitespace, pos+1)
    | isSpace t = Right (tokens, "", Whitespace, pos+1)
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos


-- Get list of tokens, append parse if there is any and reverse it
getFirst :: ([String], String, State, Int) -> [String]
getFirst (x, y, _, _) = reverse (appendToken y x)

-- Split string to tokens
parseToTokens :: String -> Either String [String]
parseToTokens str =  getFirst <$> foldM foldToTokens ([], "", Whitespace, 1) str
