module Day9 (part1, part2) where

import Lib

parsed :: IO [[Int]]
parsed = do
  inp <- lines <$> fetchInput 2023 9 "day_9.txt"
  return $ map (execParser (sepBy wsP intP)) inp


extrapolateAfter :: [Int] -> Int
extrapolateAfter xs = if all (==0) xs then 0
                 else last xs + extrapolateAfter dxs
  where
    dxs = map sub $ windows 2 xs
    sub [x, y] = y - x
    sub _ = undefined

extrapolateBefore :: [Int] -> Int
extrapolateBefore xs = if all (==0) xs then 0
                       else head xs - extrapolateBefore dxs
  where
    dxs = map sub $ windows 2 xs
    sub [x, y] = y - x
    sub _ = undefined

  
part1 :: IO ()
part1 = do
  hists <- parsed
  print $ sum $ map extrapolateAfter hists


part2 :: IO ()
part2 = do
  hists <- parsed
  print $ sum $ map extrapolateBefore hists

