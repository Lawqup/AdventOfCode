module Day3 (part1, part2) where

import Input
import Data.Char
import Lib
import Data.Maybe

parsed :: IO [String]
parsed = do
  lines <$> fetchInput 2023 3 "day_3.txt"

getPositionsMatching :: (Char -> Bool) -> [String] -> [(Int, Int)]
getPositionsMatching fpred ls =
  zip [0..] ls >>= (\(row, l) ->
                      [(row, col) | (col,x) <- zip [0..] l, fpred x])
  
keepAdj :: [String] -> [String]
keepAdj ls = zipWith keepAdjLine [0..] ls
  where
    keepAdjLine row line = map snd
                           $ filter (shouldKeep row . fst)
                           $ zip [0..] line
    shouldKeep row col = let x = ls !! row !! col in
                           not (isDigit x)
                           || checkLeft row col
                           || checkRight row col
    checkLeft row col = (col-1 >= 0
                         && isDigit (ls !! row !! (col-1))
                         && checkLeft row (col-1))
                        || isAdj row col
    checkRight row col = (col+1 < n_cols
                          && isDigit (ls !! row !! (col+1))
                          && shouldKeep row (col+1))
                         || isAdj row col
    isAdj row col = any (\(dr,dc) ->
                           let r = (row+dr)
                               c = (col+dc)
                           in (r >= 0 && r < n_rows && c >= 0 && c < n_cols && (
                                  let x = (ls !! r !! c)
                                  in not (isDigit x || x == '.'))))
                    dirs
    dirs = [(1,0), (-1,0), (0,1), (0,-1), (1,1), (-1,1), (1,-1), (-1,-1)]
    n_rows = length ls
    n_cols = length $ head ls

extractNums :: [String] -> [Int]
extractNums ls = ls >>= execParser lineP
  where
    lineP = sepP *> sepBy sepP intP
    sepP = spanP (not . isDigit)

part1 :: IO ()
part1 = do
  ls <- parsed
  print $ sum $ extractNums $ keepAdj ls

getRatios :: [String] -> Int
getRatios ls = sum $ zipWith getLineRatio [0..] ls
  where
    getLineRatio row l = sum $ zipWith (toRatio row) [0..] l
    toRatio row col x = if x /= '*' then 0 else getRatio row col

    -- TODO: overcounts due to matching multiple ways
    getRatio row col = let res = filter (>0)
                                 $ map
                             (\((dr1,dc1),(dr2,dc2)) ->
                                let r1 = row+dr1
                                    r2 = row+dr2
                                    c1 = col+dc1
                                    c2 = col+dc2
                                    v1 = if charMatches isDigit r1 c1
                                         then getInt r1 c1 else 0
                                    v2 = if charMatches isDigit r2 c2
                                         then getInt r2 c2 else 0
                                in if v1 /= v2 then v1 * v2 else 0)
                             opDirs
                       in if null res then 0 else head res

    getInt row col = if col-1 < 0
                        || not (isDigit (ls !! row !! (col - 1)))
                     then snd . fromJust $ runParser intP $ drop col $ ls !! row
                     else getInt row (col-1)
    charMatches f r c = r >= 0 && r < n_rows && c >= 0 && c < n_cols
                           && (let x = (ls !! r !! c)
                                in f x)
    dirs = [(1,0), (-1,0), (0,1), (0,-1), (1,1), (-1,1), (1,-1), (-1,-1)]
    opDirs = [((r1,c1),(r2,c2)) | (r1,c1) <- dirs,
                                  (r2, c2) <- dirs,
                                  (r1 /= r2 && c1 /= c2)
                                  || abs (c1 - c2) > 1
                                  || abs (r1 - r2) > 1]
    n_rows = length ls
    n_cols = length $ head ls


part2 :: IO ()
part2 = do
  ls <- parsed
  print $ getRatios ls
