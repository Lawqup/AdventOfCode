module Day12 (part1, part2) where

import Lib
import Data.List (intercalate)
import Data.Char (isSpace)
import Data.Bifunctor (Bifunctor(bimap))

import Data.Map (Map, (!))
import qualified Data.Map as Map

type Spring = ([Char], [Int])

parsed :: IO [Spring]
parsed = do
  ls <- lines <$> fetchInput 2023 12 "day_12.txt"
  return $ map (execParser $ (,) <$> (recordP <* wsP) <*> listP) ls
  where
    recordP = spanP (not . isSpace)
    listP = sepBy (charP ',') intP
    
type Memo = Map (Int, Int, Int) Int

validArrangements :: Spring -> Int
validArrangements (cs, list) = snd $ nValidMemo Map.empty 0 0 0
  where
    
    nValidMemo memo ci li run
      | Map.member key memo = (memo, memo ! key)
      | otherwise = let (memo', r) = nValid memo ci li run
                    in (Map.insert key r memo', r)
      where key = (ci, li, run)
      
    nValid :: Memo -> Int -> Int -> Int -> (Memo, Int)
    nValid memo ci li run
      | ci == length cs = if (li == length list && run == 0)
                             || (li == length list - 1 && run == (list !! li))
                          then (memo, 1) else (memo, 0)

      | otherwise = case cs !! ci of
                      '.' -> waysOperational memo
                      '#' -> waysBroken memo
                      '?' -> let (memo', w1) = waysOperational memo
                                 (memo'', w2) = waysBroken memo'
                             in (memo'', w1 + w2)
                      _ -> undefined
      where
        waysOperational m
          | run == 0 = nValidMemo m (ci+1) li run
          | li < length list && run == (list !! li) =
              nValidMemo m (ci+1) (li+1) 0
          | otherwise = (m, 0)
          
        waysBroken m = nValid m (ci+1) li (run+1)

part1 :: IO ()
part1 = do
  springs <- parsed
  print $ sum $ map validArrangements springs

part2 :: IO ()
part2 = do
  springs <- parsed
  let springs' = map (bimap
                      (intercalate "?" . replicate 5)
                      (concat . replicate 5))
                 springs

  print $ sum $ map validArrangements springs'
