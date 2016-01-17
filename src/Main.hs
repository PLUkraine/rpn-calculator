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

processList = fmap ( parseToRPN >=> runCalculator )
outputEither :: Either String Double -> IO ()
outputEither ( Right res ) = print res
outputEither ( Left msg ) = putStrLn msg

processCmd :: [String] ->  IO ()
processCmd = mapM_ outputEither . processList

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

