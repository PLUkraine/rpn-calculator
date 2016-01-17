module Lexer
(parseToTokens)
where

import Operators
import Data.Char(isDigit, isSpace)
import Control.Applicative((<$>))
import Control.Monad(foldM)

data State = Number | Dot | NoDotNumber

-- Appends parse to tokens' list if it isn't empty
appendToken :: String -> [String] -> [String]
appendToken parse tokens = if null parse 
                           then tokens 
                           else parse:tokens

-- Function that acts like automaton
foldToTokens :: ([ String ], String, State, Int) -> Char -> Either String ([String], String, State, Int)
foldToTokens (tokens, parse, Dot, pos) t
    | isDigit t = Right (tokens, parse ++ [t], NoDotNumber, pos+1)
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos
foldToTokens (tokens, parse, Number, pos) t
    | isDigit t = Right (tokens, parse ++ [t], Number, pos+1)
    | isDot   t = Right (tokens, parse ++ [t], Dot, pos+1)
    | isSpace t = Right (appendToken parse tokens, "", Number, pos+1)
    | isOp  [t] || isBrace t = Right ([t]:appendToken parse tokens, "", Number, pos+1)
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos
foldToTokens (tokens, parse, NoDotNumber, pos) t
    | isDigit t = Right (tokens, parse ++ [t], NoDotNumber, pos+1)
    | isSpace t = Right (appendToken parse tokens, "", Number, pos+1)
    | isOp  [t] || isBrace t = Right ([t]:appendToken parse tokens, "", Number, pos+1)
    | otherwise = Left $ "Parse error on \"" ++ [t] ++ "\" at pos " ++ show pos

-- Get list of tokens, append parse if there is any and reverse it
getFirst :: ([String], String, State, Int) -> [String]
getFirst (x, y, _, _) = reverse (appendToken y x)

-- Split string to tokens
parseToTokens :: String -> Either String [String]
parseToTokens str =  getFirst <$> foldM foldToTokens ([], "", Number, 1) str
