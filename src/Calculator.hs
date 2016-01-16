module Calculator
(runCalculator)
where
import Text.Read(readMaybe)
import Control.Monad
import Control.Applicative((<$>))
import Operators
operators = "+-*/^"

-- Function for folding the list of tokens, uses stack for evaluation
foldFunc :: [Double] -> String -> Either String [Double]
foldFunc (x:y:zs) "+" = Right $ x+y : zs
foldFunc (x:y:zs) "-" = Right $ y-x : zs
foldFunc (x:y:zs) "*" = Right $ x*y : zs
foldFunc (x:y:zs) "/" = if x /= 0 
                        then Right $ y/x : zs
                        else Left "Zero division"
foldFunc (x:y:zs) "^" = if x/=0 || y/=0
                        then Right $ y**x : zs
                        else Left "0 to power 0 is ambiguous"
foldFunc xs elem
    | isOp elem = Left $ "Couldn't apply operator " ++ elem  
            ++ ": too few arguments"
    | otherwise = case readMaybe elem of
        Just num -> Right $ num : xs
        Nothing  -> Left $ "Parse error on \"" ++ elem ++ "\"" 

-- Get result from the stack
extractResult :: Either String [Double] -> Either String Double
extractResult (Right [val]) = Right val
extractResult (Right _ ) = Left  "Invalid input expression"
extractResult (Left msg) = Left msg 

-- Evaluate RPN from String value
runCalculator :: String -> Either String Double
runCalculator = extractResult . foldM foldFunc [] . words

