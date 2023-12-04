module Day4 (part1, part2) where

import Input
import Lib

parsed :: IO [Int]
parsed = do
  ls <- lines <$> fetchInput 2023 4 "day_4.txt"
  return $ map (wins . execParser cardP) ls
  where
    cardP = stringP "Card" *> wsP *> intP *> charP ':' *> wsP
            *> ((,) <$> numsP) <*> (stringP " |" *> wsP *> numsP)
    numsP = sepBy wsP intP
    wins (winning, ours) = length $ filter (`elem` winning) ours

part1 :: IO ()
part1 = do
  cards <- parsed
  print $ sum $ map toScore cards
  where
    toScore wins = if wins > 0 then 2^(wins-1) else 0 :: Int

process :: Int -> [(Int, Int)] -> [Int] -> [Int]
process i instances cards | i == length cards = map snd instances
                          | otherwise = recurse
  where
    recurse =
      let copies = [i+w | w <- [1..(cards !! i)]]
          thisCopies = snd $ instances !! i
          update = map (\(idx,x) ->
                          if idx `elem` copies then (idx, x+thisCopies)
                          else (idx, x)
                      )
      in process (i+1) (update instances) cards

part2 :: IO ()
part2 = do
  cards <- parsed
  let instances = zip [0..] $ replicate (length cards) 1
  print $ sum $ process 0 instances cards
