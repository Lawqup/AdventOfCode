module Day17 (part1, part2) where

import Lib
import Data.Char (digitToInt)

import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Q

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

toPos :: (Int, Int) -> Dir -> (Int, Int)
toPos (r, c) dir = case dir of
                     UP -> (r-1, c)
                     DOWN -> (r+1, c)
                     LEFT -> (r, c-1)
                     RIGHT -> (r, c+1)

ccw :: Dir -> Dir
ccw dir = case dir of
            UP -> LEFT
            LEFT -> DOWN
            DOWN -> RIGHT
            RIGHT -> UP


cw :: Dir -> Dir
cw dir = case dir of
           UP -> RIGHT
           RIGHT -> DOWN
           DOWN -> LEFT
           LEFT -> UP


parsed :: IO [[Int]]
parsed = do
  ls <- lines <$> fetchInput 2023 17 "day_17.txt"
  return $ map (map digitToInt) ls


data DijkstraState = DijkstraState
  { distanceMap :: Map Node Int
  , nodeQueue :: MinPrioHeap Int Node
  } deriving Show

type Node = ((Int, Int), Dir, Int)

leastHeat :: [[Int]] -> Int -> Int -> Int
leastHeat grid minStraight maxStraight = dijkstras initial
  where
    dst = (length grid - 1, length (head grid) - 1)
    src = (0, 0)
    initial = DijkstraState
              (Map.fromList [((src, DOWN, 1), 0), ((src, RIGHT, 1), 0)])
              (Q.fromList [(0, (src, DOWN, 1)), (0, (src, RIGHT, 1))])

    dijkstras :: DijkstraState -> Int
    dijkstras (DijkstraState d q) =
      case Q.view q of
        Nothing -> undefined
        Just ((minHeat, (u, dir, prev)), q') ->
          if u == dst then minHeat
          else let neighbors =  let turn = if prev >= minStraight
                                           then [(ccw dir, 1),
                                                 (cw dir, 1)]
                                           else []
                                      
                                    fwd = ([(dir, prev + 1)
                                           | prev < maxStraight])
                                in turn ++ fwd

               in dijkstras $ foldl pushNext (DijkstraState d q') neighbors

          where
            pushNext ds@(DijkstraState d' q'') (dir', prev') =
              let next@(r, c) = toPos u dir'
              in if inBounds next grid
              then let state = (next, dir', prev')
                       heat = minHeat + grid !! r !! c

                   in if Map.notMember state d' || heat < d' ! state
                      then DijkstraState
                           (Map.insert state heat d')
                           (Q.insert (heat, state) q'')
                      else ds
              else ds


part1 :: IO ()
part1 = do
  grid <- parsed
  print $ leastHeat grid 1 3 

part2 :: IO ()
part2 = do
  grid <- parsed
  print $ leastHeat grid 4 10
