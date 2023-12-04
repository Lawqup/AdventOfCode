module Day4 (part1, part2) where

import Input
import Lib

parsed :: IO [([Int], [Int])]
parsed = do
  ls <- lines <$> fetchInput 2023 4 "day_4.txt"
  return $ map (execParser cardP) ls
  where
    cardP = stringP "Card" *> wsP *> intP *> charP ':' *> wsP
            *> ((,) <$> numsP) <*> (stringP " |" *> wsP *> numsP)
    numsP = sepBy wsP intP

wins :: ([Int], [Int]) -> Int
wins (winning, ours) = length $ filter (`elem` winning) ours

part1 :: IO ()
part1 = do
  cards <- parsed
  print $ sum $ map toScore cards
  where
    toScore x = let len = wins x
                in if len > 0 then 2^(len-1) else 0 :: Int

part2 :: IO ()
part2 = do
  cards <- map wins <$> parsed
  let subCopies i = let subs = [i+w | w <- [1..(cards !! i)]]
                    in 1 + sum (map subCopies subs) :: Int
  print $ sum $ map subCopies [0..length cards - 1]
