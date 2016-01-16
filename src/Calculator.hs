module Calculator
(runCalculator)
where
import Text.Read(readMaybe)
import Control.Monad
import Control.Applicative((<$>))
operators = "+-*/^"

-- Evaluate RPN expression using stack
foldFunc :: [Double] -> String -> Maybe [Double]
foldFunc (x:y:zs) "+" = Just $ x+y : zs
foldFunc (x:y:zs) "-" = Just $ y-x : zs
foldFunc (x:y:zs) "*" = Just $ x*y : zs
foldFunc (x:y:zs) "/" = do 
    guard (x/=0)
    return $ y/x : zs
foldFunc (x:y:zs) "^" = do
    guard $ x/=0 || y/=0
    return $ y ** x : zs
foldFunc xs elem = (:xs) <$> readMaybe elem

-- Get result from the stack
extractResult :: Maybe [Double] -> Maybe Double
extractResult (Just [val]) = Just val
extractResult _ = Nothing

-- Evaluate RPN from String value
runCalculator :: String -> Maybe Double
runCalculator = extractResult . foldM foldFunc [] . words

