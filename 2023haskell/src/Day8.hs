module Day8 (part1, part2) where
import Lib

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Dir = L | R deriving Show

type Graph = Map String (String, String)

parsed :: IO ([Dir], Graph)
parsed = do
  inp <- fetchInput 2023 8 "day_8.txt"
  return $ execParser ((,) <$> dirP <* wsP
                       <*> (Map.fromList <$> sepBy wsP nodeP)) inp
  where
    dirP = map charToDir <$> spanP (/= '\n')

    charToDir x = case x of
                    'R' -> R
                    'L' -> L
                    _ -> undefined

    nodeP = (,) <$> (spanP (/= ' ') <* stringP " = (")
            <*> ((,) <$> spanP (/= ',') <* stringP ", "
                  <*> spanP (/= ')') <* charP ')')

travel :: (String -> Bool) -> [Dir] -> Graph -> String -> Int
travel _ [] _ _ = undefined

travel endCond (d:ds) g curr = if endCond curr then 0
                               else 1 + travel endCond ds g next
  where
    next = case d of
             L -> fst $ g ! curr
             R -> snd $ g ! curr

part1 :: IO ()
part1 = do
  (dirs, g) <- parsed
  print $ travel (=="ZZZ") (cycle dirs) g "AAA"

part2 :: IO ()
part2 = do
  (dirs, g) <- parsed
  let starts = filter ((=='A') . last) $ Map.keys g
      paths = map (travel ((=='Z') . last) (cycle dirs) g) starts
  print $ foldr1 lcm paths
