module Day1 (part1, part2) where

import Input
import Data.Char

parsed :: IO [String]
parsed = do
  inp <- fetchInput 2023 1 "day_1.txt"
  return $ lines inp

part1 :: IO ()
part1 = do
  inp <- parsed
  print $ show $ sum $ map calibrationValueP1 inp

calibrationValueP1 :: String -> Int
calibrationValueP1 inp = 10 * head digits + last digits
  where
    digits = map digitToInt $ filter isDigit inp

part2 :: IO ()
part2 = do
  inp <- parsed
  print $ show $ sum $ map calibrationValueP2 inp

calibrationValueP2 :: String -> Int
calibrationValueP2 inp = 10 * head digits + last digits 
  where
    digits = parseDigits inp
    
strToInt :: String -> Maybe Int

strToInt [x] = if isDigit x then Just (digitToInt x) else Nothing

strToInt [] = Nothing

strToInt s = case s of
  "one" -> Just 1
  "two" -> Just 2
  "three" -> Just 3
  "four" -> Just 4
  "five" -> Just 5
  "six" -> Just 6
  "seven" -> Just 7
  "eight" -> Just 8
  "nine" -> Just 9
  _ -> Nothing

parseDigits :: String -> [Int]
parseDigits [] = []
parseDigits inp = let digit = parseDigit inp ""
                  in case digit of
                     Just d -> d:parseDigits (tail inp)
                     Nothing -> parseDigits (tail inp)

parseDigit :: String -> String -> Maybe Int

parseDigit [] buf = extract buf
parseDigit (x:xs) buf = case extract buf of
                       Just v -> Just v
                       Nothing -> parseDigit xs (buf ++ [x])


extract :: String -> Maybe Int
extract s@(_:xs) = case strToInt s of
                       Just v -> Just v
                       Nothing -> extract xs
extract [] = Nothing
