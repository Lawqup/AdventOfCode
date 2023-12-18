module Day16 (part1, part2) where

import Lib

import qualified Data.Set as Set

data Tile = Empty | SplitVert | SplitHori | RightMirror | LeftMirror

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

toPos :: Int -> Int -> Dir -> (Int, Int, Dir)
toPos r c dir = let (r', c') = case dir of
                                 UP -> (r-1, c)
                                 DOWN -> (r+1, c)
                                 LEFT -> (r, c-1)
                                 RIGHT -> (r, c+1)
                in (r', c', dir)

parsed :: IO [[Tile]]
parsed = do
  ls <- lines <$> fetchInput 2023 16 "day_16.txt"
  return $ map (map toTile) ls
  where
    toTile x = case x of
                 '.' -> Empty
                 '|' -> SplitVert
                 '-' -> SplitHori
                 '/' -> RightMirror
                 '\\' -> LeftMirror
                 _ -> undefined

energized :: (Int, Int, Dir) -> [[Tile]] -> Int
energized start grid = length
                       $ Set.map (\(x,y,_) -> (x,y))
                       $ energized' start Set.empty
  where
    energized' (r, c, dir) lit
      | r < 0 || r >= length grid
        || c < 0 || c >= length (head grid)
        || Set.member (r, c, dir) lit = lit
        
      | otherwise =
      let lit' = Set.insert (r, c, dir) lit
          pass dir' = energized' (toPos r c dir') lit'
          split dir1 dir2 = energized' (toPos r c dir1)
                            $ energized' (toPos r c dir2) lit'

      in case grid !! r !! c of
           Empty -> pass dir
           
           SplitVert -> if dir == RIGHT || dir == LEFT
                        then split UP DOWN
                        else pass dir
                             
           SplitHori -> if dir == UP || dir == DOWN
                        then split LEFT RIGHT
                        else pass dir
                             
           RightMirror -> pass $ case dir of
                                   LEFT -> DOWN
                                   DOWN -> LEFT
                                   RIGHT -> UP
                                   UP -> RIGHT
                             
           LeftMirror -> pass $ case dir of
                                  LEFT -> UP
                                  UP -> LEFT
                                  RIGHT -> DOWN
                                  DOWN -> RIGHT
               
part1 :: IO ()
part1 = do
  grid <- parsed
  print $ energized (0, 0, RIGHT) grid


part2 :: IO ()
part2 = do
  grid <- parsed
  let maxRow = length grid - 1
      maxCol = length (head grid) - 1
  print $ maximum $ map (`energized` grid)
    $ [(r, 0, RIGHT) | r <- [0..maxRow]]
    ++ [(r, maxCol, LEFT) | r <- [0..maxRow]] 
    ++ [(0, c, DOWN) | c <- [0..maxCol]] 
    ++ [(maxRow, c, UP) | c <- [0..maxCol]] 
