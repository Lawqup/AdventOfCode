module Lib (module Parse, module Input, bench, windows) where
import Parse
import Input
import Data.Time
import Data.List (tails)

bench :: IO a -> IO ()
bench func = do
  start <- getCurrentTime
  _ <- func
  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ "Execution Time: " ++ show diff

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails
