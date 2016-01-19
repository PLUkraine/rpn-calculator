module Main 
(main)
where
import Calculator 
import Operators
import ShuntingYardParser
import Lexer
import Control.Monad((>=>))
import Control.Applicative((<$>))
import System.Environment(getArgs)
main :: IO ()
main = do
    cmdArgs <- getArgs
    if null cmdArgs
    then interpretInput
    else processCmd cmdArgs

-- |Get list of expressions and evaluates them
processList = fmap ( parseToRPN >=> runCalculator )
-- |Output result of evaluation
outputEither :: Either String Double -> IO ()
outputEither ( Right res ) = print res
outputEither ( Left msg ) = putStrLn msg

-- |Evaluate each expression and output result
processCmd :: [String] ->  IO ()
processCmd = mapM_ outputEither . processList

-- |Ask user to type expression, evaluate it and ask for another one
interpretInput :: IO ()
interpretInput = do
    putStrLn "Enter expression"
    expr <- getLine
    case parseToRPN expr >>= runCalculator of 
        Right val -> putStrLn $ "Evaluated " ++ show val
        Left msg -> putStrLn $ "Bad expression. Error message: " ++ msg
    -- Ask user to continue
    putStrLn "Continue? y/N"
    confirm <- getLine
    if confirm /= "N" then putStrLn (replicate 20 '-') >> interpretInput
    else putStrLn "Bye"

