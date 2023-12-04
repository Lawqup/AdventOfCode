module Lib (module Parse, module Input, bench) where
import Parse
import Input
import Data.Time

bench :: IO a -> IO ()
bench func = do
  start <- getCurrentTime
  _ <- func
  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ "Execution Time: " ++ show diff
