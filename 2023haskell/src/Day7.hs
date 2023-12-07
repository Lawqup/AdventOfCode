module Day7 (part1, part2) where

import Lib
import Data.Function (on)

import Data.List (group, sort, sortBy)

newtype Hand = Hand [Int] deriving (Show, Eq)

instance Ord Hand where
  (Hand h1) <= (Hand h2) = let t1 = toType h1
                               t2 = toType h2
                           in t1 < t2 || (t1 == t2) && h1 <= h2
    where
      toType :: [Int] -> Int
      toType xs = case applyJ xs of
                    [5] -> 7
                    [1, 4] -> 6
                    [2, 3] -> 5
                    [1, 1, 3] -> 4
                    [1, 2, 2] -> 3
                    [1, 1, 1, 2] -> 2
                    _ -> 1

      applyJ xs = let xs' = filter (/= 1) xs
                      counts = sort (map length (group (sort xs')))
                      nJ = 5 - length xs'
                  in if nJ == 5 then [5]
                     else init counts ++ [last counts + nJ]

-- (Times, Distances)
parsed :: Bool -> IO [(Hand, Int)]
parsed isP2 = do
  inp <- lines <$> fetchInput 2023 7 "day_7.txt"
  return $ map (execParser ((,) <$> handP <*> (wsP *> intP))) inp
  where
    handP = Hand . map toVal <$> spanP (/= ' ')
    toVal x = case x of
                'A' -> 14
                'K' -> 13
                'Q' -> 12
                'J' -> if isP2 then 1 else 11
                'T' -> 10
                '9' -> 9
                '8' -> 8
                '7' -> 7
                '6' -> 6
                '5' -> 5
                '4' -> 4
                '3' -> 3
                '2' -> 2
                _ -> undefined

tally :: (Hand -> Hand -> Ordering) -> [(Hand, Int)] -> Int
tally comparitor hands = sum
    $ zipWith (\r (_, b) -> r * b) [1..]
    $ sortBy (comparitor `on` fst) hands

comparitorBase :: ([Int] -> [Int]) -> Hand -> Hand -> Ordering
comparitorBase apply (Hand h1) (Hand h2) = let t1 = toType h1
                                               t2 = toType h2
                                           in compare t1 t2 <> compare h1 h2
  where
    toType :: [Int] -> Int
    toType xs = case apply xs of
                  [5] -> 7
                  [1, 4] -> 6
                  [2, 3] -> 5
                  [1, 1, 3] -> 4
                  [1, 2, 2] -> 3
                  [1, 1, 1, 2] -> 2
                  _ -> 1


count :: [Int] -> [Int]
count = sort . map length . group . sort


part1 :: IO ()
part1 = do
  hands <- parsed False
  print $ tally (comparitorBase count) hands

part2 :: IO ()
part2 = do
  hands <- parsed True
  print $ tally (comparitorBase applyJ) hands
  where
      applyJ xs = let xs' = filter (/= 1) xs
                      counts = count xs'
                      nJ = 5 - length xs'
                  in if nJ == 5 then [5]
                     else init counts ++ [last counts + nJ]
