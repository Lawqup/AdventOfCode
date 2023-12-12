{-# LANGUAGE TupleSections #-}
module Lib (
  module Parse,
  module Input,
  bench,
  windows,
  replace,
  replace2,
  splitWhen,
  Memo(..),
  execMemo,
  memo,
  for3) where
import Parse
import Input
import Data.Time
import Data.List (tails, elemIndex)
import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

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


replace2 :: Int -> Int -> a -> [[a]] -> [[a]]
replace2 r c x xs = let row = xs !! r
                    in replace r (replace c x row) xs

splitWhen :: Eq a => a -> [a] -> ([a], [a])
splitWhen x xs = splitAt (fromMaybe (length xs) $ elemIndex x xs) xs

  
newtype Memo k v a = Memo { runMemo :: Map k v -> (Map k v, a) }

execMemo :: Memo k v a -> a
execMemo m = snd $ runMemo m Map.empty

instance Functor (Memo k v) where
  fmap f (Memo m) = Memo $ \cache -> let (cache', a) = m cache
                                         in (cache', f a)

instance Applicative (Memo k v) where
  pure a = Memo (, a)
  (Memo m1) <*> (Memo m2) = Memo $ \cache ->
                                     let (cache', f) = m1 cache
                                         (cache'', a) = m2 cache'
                                     in (cache'', f a)

instance Monad (Memo k v) where
  return = pure
  (Memo m1) >>= f = Memo $ \cache ->
                             let (cache', a) = m1 cache
                             in runMemo (f a) cache'
                             
memo :: Ord k => (k -> Memo k v v) -> k -> Memo k v v
memo f k = Memo $ \cache -> case Map.lookup k cache of
                              Just v -> return v
                              Nothing -> let (cache', v) = runMemo (f k) cache
                                             cache'' = Map.insert k v cache'
                                         in (cache'', v)
                                
for3 :: (((k1, k2, k3) -> mv) ->
         (k1, k2, k3) -> mv) ->
        (k1 -> k2 -> k3 -> mv) ->
        k1 -> k2 -> k3 -> mv
        
for3 m f a b c = m (\(a',b',c') -> f a' b' c') (a,b,c)
