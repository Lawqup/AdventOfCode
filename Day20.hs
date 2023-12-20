module Day20 (part1, part2) where

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Module = FlipFlop Bool [String]
            | Conjunction [Bool] [String]
            | Broadcaster [String] deriving Show


parsed :: IO (Map String Module)
parsed  = _

part1 :: IO ()
part1 = do
  undefined

part2 :: IO ()
part2 = do
  undefined
