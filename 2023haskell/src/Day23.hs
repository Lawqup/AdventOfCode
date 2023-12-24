module Day23 (part1, part2) where

import Lib

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map


import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

type Node = (Int, Int)

toPos :: (Int, Int) -> Dir -> (Int, Int)
toPos (r, c) dir = case dir of
                     UP -> (r-1, c)
                     DOWN -> (r+1, c)
                     LEFT -> (r, c-1)
                     RIGHT -> (r, c+1)

parsed :: IO (Node, Node, [[Char]])
parsed = do
  ls <- lines <$> fetchInput 2023 23 "day_23.txt"
  let sC = fromJust $ elemIndex '.' $ head ls
      dC = fromJust $ elemIndex '.' $ last ls
  return ((0, sC), (length ls - 1, dC), ls)



collapse :: (Node -> [Dir]) -> [[Char]] -> Node -> Map Node [(Int, Node)]
collapse getNeighbors grid = collapse' Map.empty
  where
    collapse' :: Map Node [(Int, Node)]
              -> Node
              -> Map Node [(Int, Node)]

    collapse' g vertex =
      let children = adjIntersections vertex
          g' = Map.insert vertex children g
      in foldl foldCollapse g' $ map snd children
      where
        foldCollapse g' child = if Map.member child g
                                then g'
                                else Map.union g' $ collapse' g' child


    adjIntersections :: Node -> [(Int, Node)]
    adjIntersections vertex =
      let visited = Set.singleton vertex
          adj = filter (\(r, c) ->
                          inBounds (r, c) grid
                          && grid !! r !! c /= '#')
                $ map (toPos vertex)
                $ getNeighbors vertex
      in map (getIntersection visited 1) adj

    getIntersection :: Set Node -> Int -> Node -> (Int, Node)
    getIntersection visited dist u =
      let visited' = Set.insert u visited
          adj = filter (\(r, c) ->
                          inBounds (r, c) grid
                          && grid !! r !! c /= '#'
                          && Set.notMember (r, c) visited')
                $ map (toPos u)
                $ getNeighbors u
      in case adj of
        [v] -> getIntersection visited' (dist + 1) v
        _ -> (dist, u)

longestPath :: Map Node [(Int, Node)] -> Node -> Node -> Int
longestPath g src dst = dfs Set.empty src
  where

    dfs :: Set Node -> Node -> Int
    dfs visited u
      | u == dst = 0
      | otherwise =
        let visited' = Set.insert u visited
            neighbors = filter (\(_, v) -> Set.notMember v visited') (g ! u)
        in maximum $ 0 : map (\(dist, v) -> dist + dfs visited' v) neighbors

part1 :: IO ()
part1 = do
  (src, dst, grid) <- parsed
  let getNeighbors (r, c) = case grid !! r !! c of
                              '>' -> [RIGHT]
                              'v' -> [DOWN]
                              '<' -> [LEFT]
                              '^' -> [UP]
                              '.' -> [DOWN, LEFT, RIGHT, UP]
                              _ -> undefined
      g = collapse getNeighbors grid src
  print $ longestPath g src dst

part2 :: IO ()
part2 = do
  (src, dst, grid) <- parsed
  let getNeighbors = const [DOWN, LEFT, RIGHT, UP]
      g = collapse getNeighbors grid src
  print $ longestPath g src dst
