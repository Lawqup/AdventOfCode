module Day14 (part1, part2) where

import Lib
import Data.List (transpose)
import Debug.Trace (trace)

import Data.Map (Map, (!))
import qualified Data.Map as Map

parsed :: IO [[Char]]
parsed = lines <$> fetchInput 2023 14 "day_14.txt"

tiltWest :: [[Char]] -> [[Char]]
tiltWest = map tiltRow
  where

    tiltRow [] = []
    tiltRow rs = let (l, r) = splitWhen '#' rs
                     rounded = length $ filter (=='O') l
                     maybeCube = ['#' | not $ null r]
                     maybeRest = if not $ null r then tiltRow (tail r) else []
                 in replicate rounded 'O'
                    ++ replicate (length l - rounded) '.'
                    ++ maybeCube
                    ++ maybeRest

tiltNorth :: [[Char]] -> [[Char]]
tiltNorth = transpose . tiltWest . transpose

tiltEast :: [[Char]] -> [[Char]]
tiltEast = map reverse . tiltWest . map reverse

tiltSouth :: [[Char]] -> [[Char]]
tiltSouth = reverse . tiltNorth . reverse

cycleTilt :: Int -> [[Char]] -> [[Char]]
cycleTilt it | it == 0 = id
             | otherwise = cycleTilt' Map.empty it
  where
    cycleTilt' :: Map [[Char]] Int -> Int -> [[Char]] -> [[Char]]
    cycleTilt' cache n plat
      | n == 0 = plat
      | Map.member plat cache = let period = (cache ! plat) - n
                                in cycleTilt (n `mod` period) plat
      | otherwise = progress $ let cache' = Map.insert plat n cache
                    in cycleTilt' cache' (n - 1) $ cycleOnce plat
      where 
        progress = if n `mod` 1 == 0
                   then trace $ "At " ++ show n ++ " cycles!"
                   else id
                        
    cycleOnce = tiltEast . tiltSouth . tiltWest . tiltNorth

load :: [[Char]] -> Int
load platform = sum $ zipWith rowLoad [len, len-1..] platform
  where
    len = length platform
    rowLoad distFromSouth row = (* distFromSouth) $ length $ filter (=='O') row

part1 :: IO()
part1 = do
  platform <- parsed
  print $ load $ tiltNorth platform


part2 :: IO()
part2 = do
  platform <- parsed
  -- putStrLn $ unlines $ cycleTilt 10000 platform
  print $ load $ cycleTilt 1000000000 platform
