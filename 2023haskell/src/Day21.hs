module Day21 (part1, part2) where

import Lib

import Data.Set (Set)
import qualified Data.Set as Set

data Tile = Rock | Plot deriving (Eq, Show)

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

toPos :: Int -> Int -> Dir -> (Int, Int)
toPos r c dir = let (r', c') = case dir of
                                 UP -> (r-1, c)
                                 DOWN -> (r+1, c)
                                 LEFT -> (r, c-1)
                                 RIGHT -> (r, c+1)
                in (r', c')

parsed :: IO ((Int, Int), [[Tile]])
parsed = do
  ls <- lines <$> fetchInput 2023 21 "day_21.txt"
  let (col, row, _) = head [(r, c, t) |
                             (r, ro) <- zip [0..] ls,
                             (c, t) <- zip [0..] ro,
                             t == 'S']
      grid = map (map toTile) ls
  return ((row, col), grid)
  where
    toTile x = case x of
                 '.' -> Plot
                 'S' -> Plot
                 '#' -> Rock
                 _ -> error $ "undefined char " ++ show x

diffuse :: Int -> (Int, Int) -> [[Tile]] -> Int
diffuse steps start grid = let queue = [(start, steps)]
                               visited = Set.singleton start
                               ans = Set.empty
                           in Set.size $ bfs visited ans queue
  where
    bfs :: Set (Int, Int)
        -> Set (Int, Int)
        -> [((Int, Int), Int)]
        -> Set (Int, Int)
    bfs _ res [] = res
    bfs seen res (((r, c), s):q) =
      let res' = if even s
                 then Set.insert (r, c) res
                 else res
      in if s == 0
         then bfs seen res' q
         else let next = filter (\(r', c') ->
                                    inBounds (r', c') grid
                                    && grid !! r' !! c' /= Rock
                                    && Set.notMember (r', c') seen)
                         $ map (toPos r c) [UP, DOWN, LEFT, RIGHT]
                  (seen', q') = foldl
                    (\(seen'', q'') pos ->
                        (Set.insert pos seen'', q'' ++ [(pos, s-1)]))
                    (seen, q) next
              in bfs seen' res' q'


part1 :: IO ()
part1 = do
  (start, grid) <- parsed
  print $ diffuse 64 start grid

part2 :: IO ()
part2 = do
  (start@(r, c), grid) <- parsed
  let diffuse' steps' start' = diffuse steps' start' grid
      steps = 26501365
      size = length grid
      hypergrid_width = (steps `div` size) - 1

      odd_grids = ((hypergrid_width `div` 2) * 2 + 1) ^ (2 :: Integer)
      even_grids = (((hypergrid_width + 1) `div` 2) * 2) ^ (2 :: Integer)

      odd_points = diffuse' (size * 2 + 1) start
      even_points = diffuse' (size * 2) start

      corner_steps = size - 1
      corner_t = diffuse' corner_steps (size - 1, c)
      corner_b = diffuse' corner_steps (0, c)
      corner_l = diffuse' corner_steps (r, size - 1)
      corner_r = diffuse' corner_steps (r, 0)

      small_steps = size `div` 2 - 1
      small_tr = diffuse' small_steps (size - 1, 0)
      small_tl = diffuse' small_steps (size - 1, size - 1)
      small_br = diffuse' small_steps (0, 0)
      small_bl = diffuse' small_steps (0, size - 1)

      large_steps = (3 * size) `div` 2 - 1
      large_tr = diffuse' large_steps (size - 1, 0)
      large_tl = diffuse' large_steps (size - 1, size - 1)
      large_br = diffuse' large_steps (0, 0)
      large_bl = diffuse' large_steps (0, size - 1)
      
  print
    $ odd_grids * odd_points
    + even_grids * even_points
    + corner_t + corner_b + corner_l + corner_r
    + (hypergrid_width + 1) * (small_tr + small_tl + small_br + small_bl)
    + hypergrid_width * (large_tr + large_tl + large_br + large_bl)
    
