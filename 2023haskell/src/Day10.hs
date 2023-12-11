module Day10 (part1, part2) where

import Lib
import Data.List (elemIndices, find)
import Data.Maybe (fromJust, isJust)

parsed :: IO [[Char]]
parsed = do
  lines <$> fetchInput 2023 10 "day_10.txt"

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

valid :: Dir -> [Char]
valid UP = ['|', 'F', '7']
valid DOWN = ['|', 'L', 'J']
valid LEFT = ['-', 'F', 'L']
valid RIGHT = ['-', 'J', '7']

validNeighbors :: Char -> [Dir]
validNeighbors x = case x of
                     '|' -> [UP, DOWN]
                     '-' -> [LEFT, RIGHT]
                     'L' -> [UP, RIGHT]
                     'J' -> [UP, LEFT]
                     '7' -> [DOWN, LEFT]
                     'F' -> [DOWN, RIGHT]
                     'S' -> [UP, DOWN, LEFT, RIGHT]
                     _ -> undefined


toPos :: Int -> Int -> Dir -> (Int, Int)
toPos r c dir = case dir of
                  UP -> (r-1, c)
                  DOWN -> (r+1, c)
                  LEFT -> (r, c-1)
                  RIGHT -> (r, c+1)

getLoop :: [[Char]] -> [(Int, Int)]
getLoop pipes = let start = head
                      [(r, c) | (r, row) <- zip [0..] pipes, c <- elemIndices 'S' row]
                    in getLoopR (0, 0) start
  where
    getLoopR (pr, pc) (cr, cc) = if pipes !! nr !! nc == 'S' then [(cr, cc)]
               else
                 (cr,cc) : getLoopR (cr, cc) (nr, nc)
      where
        (nr, nc) = fromJust . fromJust
                   $ find isJust
                   $ map pipe
                   $ validNeighbors
                   $ pipes !! cr !! cc

        pipe dir = let (r, c) = toPos cr cc dir in
                     if not (r == pr && c == pc)
                        && r >= 0 && r < length pipes
                        && c >= 0 && c < length (head pipes)
                        && pipes !! r !! c `elem` 'S' : valid dir
                     then Just (r, c)
                     else Nothing


showLoop :: [[Char]] -> [(Int, Int)] -> String
showLoop pipes xs = unlines $ fill xs
  where
    fill [] = replicate maxRow (replicate maxCol '.')
    fill ((r, c):ys) = let char = case pipes !! r !! c of
                                    '|' -> '║'
                                    '-' -> '═'
                                    'L' -> '╚'
                                    'J' -> '╝'
                                    '7' -> '╗'
                                    'F' -> '╔'
                                    'S' -> 'S'
                                    _ -> undefined
                       in replace2 r c char $ fill ys


    maxRow = succ $ maximum $ map fst xs
    maxCol = succ $ maximum $ map snd xs


enclosed :: [(Int, Int)] -> Int
enclosed loop = (doubleArea - length loop) `div` 2 + 1
  where
    doubleArea = abs . sum . map area . windows 2 $ loop ++ [head loop]
    area [(x1, y1), (x2, y2)] = x1 * y2 - x2 * y1
    area _ = undefined

part1 :: IO ()
part1 = do
  pipes <- parsed
  print . ceiling . (/2) . fromIntegral . length $ getLoop pipes

part2 :: IO ()
part2 = do
  pipes <- parsed
  let loop = getLoop pipes
  putStrLn $ showLoop pipes loop
  print $ enclosed loop
