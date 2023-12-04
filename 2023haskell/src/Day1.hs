module Day1 (part1, part2) where

import Input
import Data.Char
import Lib
import Data.Functor (($>))
import Control.Applicative


parsed :: IO [String]
parsed = do
  inp <- fetchInput 2023 1 "day_1.txt"
  return $ lines inp

combine :: [Int] -> Int
combine digits = 10 * head digits + last digits

part1 :: IO ()
part1 = do
  inp <- parsed
  print $ sum $ map (combine . map digitToInt . filter isDigit) inp

part2 :: IO ()
part2 = do
  inp <- parsed
  print $ sum $ map (combine . parseDigits) inp
  where
    parseDigits s@(_:xs) = let rest = parseDigits xs
                           in maybe rest (:rest) (maybeExecParser numP s)
    parseDigits [] = []

    numP = digitP
           <|> (stringP "one"   $> 1) 
           <|> (stringP "two"   $> 2) 
           <|> (stringP "three" $> 3) 
           <|> (stringP "four"  $> 4) 
           <|> (stringP "five"  $> 5) 
           <|> (stringP "six"   $> 6) 
           <|> (stringP "seven" $> 7) 
           <|> (stringP "eight" $> 8) 
           <|> (stringP "nine"  $> 9) 

