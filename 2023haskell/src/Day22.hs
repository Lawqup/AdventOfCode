{-# LANGUAGE TupleSections #-}
module Day22 (part1, part2) where

import Lib
import Data.List (sortOn, elemIndex)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Coord = (Int, Int, Int)
type Brick = [Coord]

getZ :: Coord -> Int
getZ (_, _, z) = z

parsed :: IO [Brick]
parsed = do
  ls <- lines <$> fetchInput 2023 22 "day_22.txt"
  return $ map (execParser brickP) ls
  where
    brickP = toBrick <$> coordP <* charP '~' <*> coordP
    coordP = (,,) <$> intP <* charP ',' <*> intP <* charP ',' <*> intP

    toBrick (x1, y1, z1) (x2, y2, z2) =
      [(x, y, z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]


settle :: [Brick] -> (Map Int [Int], Map Int [Int], [Brick])
settle bricks = let sorted = sortOn (minimum . map getZ) bricks
                    emp = Map.fromList $ map (, []) [0..length bricks - 1]
                in buildGraph emp emp sorted 0
  where
    buildGraph :: Map Int [Int]
               -> Map Int [Int]
               -> [Brick]
               -> Int
               -> (Map Int [Int], Map Int [Int], [Brick])

    buildGraph under above bs i
      | i >= length bs = (under, above, bs)
      | otherwise = let (under', above', bs') =
                          moveDown under above bs i
                    in buildGraph under' above' bs' (i + 1)

    moveDown :: Map Int [Int]
             -> Map Int [Int]
             -> [Brick]
             -> Int
             -> (Map Int [Int], Map Int [Int], [Brick])
    moveDown under above bs i =
      let brick = bs !! i
          below = [idx | idx <- [0.. i - 1],
                    any (\(ox, oy, oz) ->
                            any (\(cx, cy, cz) ->
                                    cz == oz + 1
                                    && cx == ox
                                    && cy == oy)
                            brick) $ bs !! idx]
      in if null below && (minimum . map getZ) brick > 1
         then let brick' = map (\(x, y, z) -> (x, y, z - 1)) brick
                  bs' = replace i brick' bs
              in moveDown under above bs' i

         else let under' = Map.insert i below under
                  foldAbove above'' brickAb =
                    let old = above'' ! brickAb
                    in Map.insert brickAb (old ++ [i]) above''
                  above' = foldl foldAbove above below
              in (under', above', bs)
  
part1 :: IO ()
part1 = do
  bricks <- parsed
  let (under, above, _) = settle bricks
  -- print above
  print
    $ length
    $ filter (\brick -> all ((> 1) . length . (under !)) (above ! brick)) [0..length bricks - 1]

chainReaction :: Map Int [Int] -> Map Int [Int] -> Int -> Int
chainReaction under above toRemove = bfs under [toRemove]
  where
    bfs :: Map Int [Int] -> [Int] -> Int
    bfs _ [] = 0
    bfs u (i:is) = let children = above ! i
                       u' = foldl foldRemove u children
                       fallen = filter (null . (u' !)) children
                       
                   in length fallen + bfs u' (is ++ fallen)
      where 
        foldRemove :: Map Int [Int] -> Int -> Map Int [Int]
        foldRemove u'' child = let parents = u'' ! child
                                   idx = fromJust $ elemIndex i parents
                                   parents' = delete idx parents
                               in Map.insert child parents' u''
        
part2 :: IO ()
part2 = do
  (under, above, bricks) <- settle <$> parsed
  print
    $ sum
    $ map (chainReaction under above) [0..length bricks - 1]
  
