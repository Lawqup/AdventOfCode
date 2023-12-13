module Day13 (part1, part2) where

import Lib
import Data.List (transpose, find)
import Data.Maybe (fromJust)
import Control.Applicative

type Pattern = [[Char]]

parsed :: IO [Pattern]
parsed = do
  inp <- fetchInput 2023 13 "day_13.txt"
  return $ execParser (map lines <$> sepBy sep (untilP sep)) inp
  where
    sep = stringP "\n\n"

symetric :: Int -> Pattern -> Int
symetric dist p = fromJust $ colI <|> ((* 100) <$> rowI)
  where
    colI = symIndex p
    rowI = symIndex $ transpose p
    symIndex p' = find (\n -> reflectionScore n p' == dist)
                  [1 .. length (head p') - 1]

    reflectionScore n = sum . map rowScore
      where
        rowScore row = let (l, r) = splitAt n row
                       in length $ filter not $ zipWith (==) (reverse l) r

part1 :: IO ()
part1 = do
  pats <- parsed
  print $ sum $ map (symetric 0) pats


part2 :: IO ()
part2 = do
  pats <- parsed
  print $ sum $ map (symetric 1) pats
