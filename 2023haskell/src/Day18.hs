{-# LANGUAGE TupleSections #-}
module Day18 (part1, part2) where

import Lib
import Text.ParserCombinators.ReadP (between)
import Control.Applicative
import Data.List (minimumBy, maximumBy)
import Data.Function (on)

import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShowId, traceShow)

type Instr = (Dir, Int)

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

toPos :: Int -> Int -> Dir -> (Int, Int)
toPos r c dir = case dir of
                  UP -> (r-1, c)
                  DOWN -> (r+1, c)
                  LEFT -> (r, c-1)
                  RIGHT -> (r, c+1)


toPosN :: Int -> Int -> Int -> Dir -> (Int, Int)
toPosN r c n dir = case dir of
                     UP -> (r-n, c)
                     DOWN -> (r+n, c)
                     LEFT -> (r, c-n)
                     RIGHT -> (r, c+n)

parsed :: Bool -> IO [Instr]
parsed isHex = do
  ls <- lines <$> fetchInput 2023 18 "day_18.txt"
  return $ map (execParser $ if isHex then hexP else instrP) ls
  where
    instrP = (,) <$> (dirP <* wsP)
             <*> (intP <* wsP)

    hexP = instrP *> stringP "(#" *> (toInstr <$> spanP (/= ')')) <* charP ')'

    toInstr xs = (digitToDir (last xs), parseHex (init xs))

    digitToDir x = case x of
                     '0' -> RIGHT
                     '1' -> DOWN
                     '2' -> LEFT
                     '3' -> UP
                     _ -> undefined

    dirP = (RIGHT <$ charP 'R')
           <|> (UP <$ charP 'U')
           <|> (DOWN <$ charP 'D')
           <|> (LEFT <$ charP 'L')


getTrench :: [Instr] -> Set (Int, Int)
getTrench = trench 0 0
  where
    trench _ _ [] = Set.empty
    trench r c ((dir, n):is) =
      let (r', c') = toPosN r c n dir
          new = [(r'', c'') |
                  r'' <- [min r r'..max r r'],
                  c'' <- [min c c'..max c c']]
      in Set.union
         (Set.fromList new)
         (trench r' c' is)
                                      


filled :: Set (Int, Int) -> Int
filled trench = (1 + maxR - minR) * (1 + maxC - minC) -
                length (fillOuter Set.empty minR minC)
  where
    minR = minimum (Set.map fst trench) - 1
    maxR = maximum (Set.map fst trench) + 1
    minC = minimum (Set.map snd trench) - 1
    maxC = maximum (Set.map snd trench) + 1

    fillOuter :: Set (Int, Int) -> Int -> Int -> Set (Int, Int)
    fillOuter out r c
      | Set.member (r, c) out
        || Set.member (r, c) trench
        || r < minR || r > maxR
        || c < minC || c > maxC = out
      | otherwise = let out' = Set.insert (r, c) out
                    in foldl foldFill out'
                       $ map (toPos r c) [UP, DOWN, LEFT, RIGHT]
      where
        foldFill out'' (r', c') = fillOuter out'' r' c'

part1 :: IO ()
part1 = do
  trench <- getTrench <$> parsed False
  print $ filled trench

part2 :: IO ()
part2 = do
  trench <- getTrench <$> parsed True
  print $ filled trench

