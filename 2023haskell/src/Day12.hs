{-# LANGUAGE LambdaCase #-}

module Day12 (part1, part2) where

import Lib
import Data.List (group, permutations, find, elemIndex, intercalate)
import Data.Char (isSpace)
import Debug.Trace (traceShow, trace, traceShowId)
import Data.Bits (Bits(shiftR))
import Data.Bifunctor (Bifunctor(bimap))

type Spring = ([Char], [Int])

parsed :: IO [Spring]
parsed = do
  ls <- lines <$> fetchInput 2023 12 "day_12.txt"
  return $ map (execParser $ (,) <$> (recordP <* wsP) <*> listP) ls
  where
    recordP = spanP (not . isSpace)
    listP = sepBy (charP ',') intP

validArrangements :: Spring -> Int
validArrangements (conds, list) = nValid conds list
  where
    nValid cs 0 = if isValid cs list then 1 else 0
      
    isValid ls cs = case (dropWhile (=='.') cs, ls) of
                      ([], []) -> True
                      (cs', l:ls') -> let (b, cs'') = splitWhen '.' cs'
                                      in length b == l && isValid ls' cs''
                      (_, _) -> False

    allConds [] = []
    allConds cs = let nUnknown = length $ filter (=='?') cs
                  in map (genRecord cs) [0 :: Int .. 2 ^ nUnknown - 1]
                     
    genRecord cs 0 = map (\x -> if x == '?' then '.' else x) cs

    genRecord ('?' : cs) n = let x = if odd n then '#' else '.'
                             in x : genRecord cs (n `shiftR` 1)

    genRecord cs n = let (l, cs') = splitWhen '?' cs
                     in l ++ genRecord cs' n

part1 :: IO ()
part1 = do
  springs <- parsed
  print $ sum $ map validArrangements springs

part2 :: IO ()
part2 = do
  springs <- parsed
  let springs' = map (bimap
                      (intercalate "?" . replicate 5)
                      (concat . replicate 5))
                 springs
                 
  print $ sum $ map validArrangements springs'
