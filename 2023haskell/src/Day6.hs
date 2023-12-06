module Day6  (part1, part2) where

import Lib

-- (Times, Distances)
parsed :: IO ([Int], [Int])
parsed = do
  inp <- fetchInput 2023 6 "day_6.txt"
  return $ execParser ((,)
                        <$> (stringP "Time:" *> wsP *> sepBy wsP intP
                             <* charP '\n')
                        <*> (stringP "Distance:" *> wsP *> sepBy wsP intP)) inp


ways :: Int -> Int -> Int
ways time dist = hi - lo
  where
    timef :: Float
    timef = fromIntegral time
    distf :: Float
    distf = fromIntegral (dist + 1)
    discriminant = sqrt $ timef ** 2 - 4 * distf
    hi = floor $ (timef + discriminant) / 2.0
    lo = floor $ (timef - discriminant) / 2.0

part1 :: IO()
part1 = do
  (times, distances) <- parsed
  print $ product $ zipWith ways times distances


part2 :: IO()
part2 = do
  (times, distances) <- parsed
  let time = conc times
      dist = conc distances
  print $  ways time dist
  where
    conc = read . foldl (\prev x -> prev ++ show x) ""
