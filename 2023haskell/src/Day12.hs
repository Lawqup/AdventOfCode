{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Day12 (part1, part2) where

import Lib
    ( charP,
      execParser,
      intP,
      sepBy,
      spanP,
      wsP,
      fetchInput)
import Data.List (group, permutations, find, elemIndex, intercalate)
import Data.Char (isSpace)
import Data.Bifunctor (Bifunctor(bimap))

import Control.Monad.Memo

type Spring = ([Char], [Int])

parsed :: IO [Spring]
parsed = do
  ls <- lines <$> fetchInput 2023 12 "day_12.txt"
  return $ map (execParser $ (,) <$> (recordP <* wsP) <*> listP) ls
  where
    recordP = spanP (not . isSpace)
    listP = sepBy (charP ',') intP

validArrangements :: Spring -> Int
validArrangements (cs, list) = startEvalMemo $ nValid 0 0 0
  where
    nValid :: (MonadMemo (Int, Int, Int) Int m) => Int -> Int -> Int -> m Int 
    nValid ci li run
      | ci == length cs = if (li == length list && run == 0)
                             || (li == length list - 1 && run == (list !! li))
                          then return 1 else return 0

      | otherwise = case cs !! ci of
                      '.' -> waysOperational
                      '#' -> waysBroken
                      '?' -> do
                        w1 <- waysOperational
                        w2 <- waysBroken
                        return (w1 + w2)
                      _ -> undefined
      where
        waysOperational
          | run == 0 = for3 memo nValid (ci+1) li run
          | li < length list && run == (list !! li) =
              for3 memo nValid (ci+1) (li+1) 0
          | otherwise = return 0
          
        waysBroken = for3 memo nValid (ci+1) li (run+1)

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
