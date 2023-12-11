module Day11 (part1, part2) where

import Lib
import Data.List (nub, sort, tails)

type Stars = [(Int, Int)]

toGalaxy :: [[Char]] -> Stars
toGalaxy g = filter (\(r, c) -> (g !! r !! c) == '#')
             $ [(r, c) |
                 r <- [0..length g - 1],
                 c <- [0..length (head g) - 1]]

parsed :: IO Stars
parsed = do
  toGalaxy . lines <$> fetchInput 2023 11 "day_11.txt"

expand :: Int -> Stars -> Stars
expand n g = map expandG g
  where
    expandG (r, c) = (r + expandBy r emptyRows, c + expandBy c emptyCols)
    expandBy r xs = ((n-1)*) $ length $ filter (< r) xs

    emptyRows = (windows 2 . nub . sort $ map fst g) >>= toEmpty
    emptyCols = (windows 2 . nub . sort $ map snd g) >>= toEmpty

    toEmpty [x1, x2] = [x1 + 1.. x2 - 1]
    toEmpty _ = undefined

dist :: (Int, Int) -> (Int, Int) -> Int
dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

part1 :: IO ()
part1 = do
  stars <- expand 2 <$> parsed
  print
    $ sum . map (uncurry dist)
    $ [(g1, g2) | (g1:g2s) <- tails stars, g2 <- g2s]

part2 :: IO ()
part2 = do
  stars <- expand 1000000 <$> parsed
  print
    $ sum . map (uncurry dist)
    $ [(g1, g2) | (g1:g2s) <- tails stars, g2 <- g2s]
