module Day3 (part1, part2) where

import Input
import Data.Char
import Lib
import Data.Bifunctor (bimap)
import Data.List (nub)

parsed :: IO [String]
parsed = do
  lines <$> fetchInput 2023 3 "day_3.txt"

getPositionsMatching :: (Char -> Bool) -> [String] -> [(Int, Int)]
getPositionsMatching fpred ls =
  zip [0..] ls >>= (\(row, l) ->
                      [(row, col) | (col,x) <- zip [0..] l, fpred x])

getAdj :: Int -> Int -> [String] -> [Int]
getAdj row col ls = nub
                    $ map (\(r,c) -> getInt r c ls)
                    $ filter (\(r,c) -> charMatches isDigit r c ls)
                    $ map (bimap (row +) (col +)) dirs

getRatio :: Int -> Int -> [String] -> Int
getRatio row col ls = let res = filter (>0)
                                $ map
                                (\((dr1,dc1),(dr2,dc2)) ->
                                   let r1 = row+dr1
                                       r2 = row+dr2
                                       c1 = col+dc1
                                       c2 = col+dc2
                                       v1 = if charMatches isDigit r1 c1 ls
                                            then getInt r1 c1 ls else 0
                                       v2 = if charMatches isDigit r2 c2 ls
                                            then getInt r2 c2 ls else 0
                                   in if v1 /= v2 then v1 * v2 else 0)
                                dirs2
                      in if null res then 0 else head res
  where
    dirs2 = [((r1,c1),(r2,c2)) | (r1,c1) <- dirs,
                                 (r2, c2) <- dirs]


getInt :: Int -> Int -> [String] -> Int
getInt row col ls = if col-1 < 0
                       || not (isDigit (ls !! row !! (col - 1)))
                    then execParser intP $ drop col $ ls !! row
                    else getInt row (col-1) ls
                         
charMatches :: (Char -> Bool) -> Int -> Int -> [String] -> Bool
charMatches f r c ls = r >= 0 && r < n_rows && c >= 0 && c < n_cols
                       && (let x = (ls !! r !! c)
                            in f x)
  where
    n_rows = length ls
    n_cols = length $ head ls


dirs :: [(Int, Int)]
dirs = [(1,0), (-1,0), (0,1), (0,-1), (1,1), (-1,1), (1,-1), (-1,-1)]

part1 :: IO ()
part1 = do
  ls <- parsed
  let symbols = getPositionsMatching (\x -> not (isDigit x || x == '.')) ls
  print $ sum $ symbols >>= (\(row, col) -> getAdj row col ls)

part2 :: IO ()
part2 = do
  ls <- parsed
  print $ sum $ map
    (\(row, col) -> getRatio row col ls) $ getPositionsMatching (=='*') ls
