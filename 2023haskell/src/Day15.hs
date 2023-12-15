module Day15 (part1, part2) where

import Lib
import Data.List (elemIndex)
import Data.Char (ord)
import Control.Applicative

import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Op = Add Int | Remove

parsed :: IO [String]
parsed = execParser (sepBy (charP ',') (spanP (/= ','))) . filter (/='\n')
         <$> fetchInput 2023 15 "day_15.txt"

parsed2 :: IO [(String, Op)]
parsed2 = do
  inp <- filter (/='\n') <$> fetchInput 2023 15 "day_15.txt"
  return $ execParser (sepBy (charP ',') opP) inp
  where
    opP = (,) <$> untilP (charP '-' <|> charP '=')
          <*> ((Add <$> (charP '=' *> intP)) <|> (Remove <$ charP '-'))


hash :: String -> Int
hash = foldl hashChar 0
  where
    hashChar currentValue c = (`mod` 256) $ (*17) $ currentValue + ord c

type Boxes = Map Int [(String, Int)]

doOp :: Boxes -> (String, Op) -> Boxes
doOp boxes (label, op) = let k = hash label
                             prev = fromMaybe [] $ boxes !? k
                             idx = elemIndex label $ map fst prev
                             prev' = case (op, idx) of
                               (Add f, Just i) -> replace i (label, f) prev
                               (Add f, Nothing) -> prev ++ [(label, f)]
                               (Remove, Just i) -> delete i prev
                               (Remove, Nothing) -> prev
                          in Map.insert k prev' boxes
part1 :: IO ()
part1 = do
  inp <- parsed
  print $ sum $ map hash inp

  
part2 :: IO ()
part2 = do
  inp <- parsed2
  let boxes = foldl doOp Map.empty inp
  print $ sum $ Map.mapWithKey focusingPower boxes
  where
    focusingPower :: Int -> [(String, Int)] -> Int
    focusingPower n box = sum
                          $ zipWith (\i x -> i * x * (n+1))
                          [1..] $ map snd box

