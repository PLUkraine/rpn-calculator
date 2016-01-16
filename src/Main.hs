module Main 
(main)
where
import Calculator 
import Operators
import ShuntingYardParser
import Control.Monad((>=>))
import Control.Applicative((<$>))
import System.Environment(getArgs)
main :: IO ()
main = do
    cmdArgs <- getArgs
    if null cmdArgs
    then interpretInput
    else processCmd cmdArgs

processList :: [String] -> [Maybe Double]
processList = fmap ( parseToRPN >=> runCalculator )


processCmd :: [String] ->  IO ()
processCmd = mapM_ print . processList

interpretInput :: IO ()
interpretInput = do
    putStrLn "Enter expression"
    expr <- getLine
    case parseToRPN expr >>= runCalculator of 
        Just val -> putStrLn $ "Evaluated " ++ show val
        Nothing  -> putStrLn "Bad expression"
    -- Ask user to continue
    putStrLn "Continue? y/N"
    confirm <- getLine
    if confirm /= "N" then interpretInput
    else putStrLn "Bye"

