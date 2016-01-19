module Main (
    main
 ) where
 
import qualified Test.Framework as T
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Lexer
import Data.Text (pack, count)
import Data.Char (isSpace)
import Data.Either(isLeft)

main :: IO ()
main = T.defaultMain tests

tests :: [T.Test]
tests =
  [
    testProperty "No spaces in produced tokens" noSpacesInTokens,
    testProperty "No stray dots in produced" noStrayDotsInTokens
  ]

noStrayDotsInTokens :: String -> Bool
noStrayDotsInTokens str = applyPredOrTrue (not . or . map isDot) tokens 
    where
        tokens = parseToTokens str
        isDot s = s == "."

func someProp = quickCheckWith stdArgs { maxSuccess = 5000 } someProp

noSpacesInTokens :: String -> Bool
noSpacesInTokens str = applyPredOrTrue (not . or . map isSpaceInString) tokens 
    where
        tokens = parseToTokens str
        isSpaceInString = or . map isSpace
         
-- Auxiliary functions

applyPredOrTrue :: (r -> Bool) -> Either l r -> Bool
applyPredOrTrue pred (Left _) = True
applyPredOrTrue pred (Right val) = pred val

countOccurences :: String -> String -> Int
countOccurences haystack needle = go needle
     where 
        packed = pack haystack
        go needle = count (pack needle) packed
