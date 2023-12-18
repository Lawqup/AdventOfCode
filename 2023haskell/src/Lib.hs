module Lib (
  module Parse,
  module Input,
  bench,
  windows,
  replace,
  replace2,
  delete,
  splitWhen,
  inBounds,
  parseHex) where
import Parse
import Input
import Data.Time
import Data.List (tails, elemIndex, foldl')
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)

bench :: IO a -> IO ()
bench func = do
  start <- getCurrentTime
  _ <- func
  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ "Execution Time: " ++ show diff

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

delete :: Int -> [a] -> [a]
delete i xs = take i xs ++ drop (i+1) xs


replace2 :: Int -> Int -> a -> [[a]] -> [[a]]
replace2 r c x xs = let row = xs !! r
                    in replace r (replace c x row) xs

splitWhen :: Eq a => a -> [a] -> ([a], [a])
splitWhen x xs = splitAt (fromMaybe (length xs) $ elemIndex x xs) xs

inBounds :: (Int, Int) -> [[a]] -> Bool
inBounds (r, c) grid = r >= 0 && r < length grid
                       && c >= 0 && c < length (head grid)

parseHex :: String -> Int
parseHex = foldl' f 0
  where
    f n c = 16*n + hexChar c
    hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $
                 elemIndex (toUpper ch) "0123456789ABCDEF"
