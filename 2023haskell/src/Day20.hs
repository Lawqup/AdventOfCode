{-# LANGUAGE TupleSections #-}
module Day20 (part1, part2) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Lib
import Control.Applicative
import Data.Functor (($>))
import Debug.Trace (traceShow)

data Module = FlipFlop Bool [String]
            | Conjunction (Map String Signal) [String]
            | Broadcaster [String] deriving Show

data Signal = Low | High | Ignore deriving (Eq, Show)

children :: Module -> [String]
children (FlipFlop _ cs) = cs
children (Conjunction _ cs) = cs
children (Broadcaster cs) = cs

parsed :: IO (Map String Module)
parsed  = do
  ls <- lines <$> fetchInput 2023 20 "day_20.txt"
  let mods = Map.fromList $ map (execParser modP) ls
  return $ Map.mapWithKey (setParents mods) mods
  where
    modP :: Parser (String, Module)
    modP = do
      modBuilder <- charP '%' $> FlipFlop False
                    <|> charP '&' $> Conjunction Map.empty
                    <|> pure Broadcaster

      name <- spanP (/= ' ')
      _ <- stringP " -> "
      cs <- sepBy (stringP ", ") (spanP (/= ','))
      return (name, modBuilder cs)


    setParents :: Map String Module -> String -> Module -> Module
    setParents mods name (Conjunction _ cs) =
      let parents = map fst
                    $ filter (elem name . snd)
                    $ Map.assocs
                    $ Map.map children mods
      in Conjunction (Map.fromList $ map (, Low) parents) cs
    setParents _ _ mod' = mod'

processSignal :: String -> Signal -> Module -> (Signal, Module)
processSignal _ Ignore _ = error "Tried processing ignore signal"
processSignal _ sig m@(Broadcaster _) = (sig, m)
processSignal _ sig m@(FlipFlop state cs) =
  case (sig, state) of
    (High, _) -> (Ignore, m)
    (Low, False) -> (High, FlipFlop True cs)
    (Low, True) -> (Low, FlipFlop False cs)
processSignal from sig (Conjunction state cs) =
  let state' = Map.insert from sig state
      out = if all (== High) $ Map.elems state' then Low else High
   in (out, Conjunction state' cs)

pushButton :: Int -> Map String Module -> (Map String Module, Int, Int)
pushButton n modules = let q = [("broadcaster", "", Low)]
                     in bfs modules q
  where
    bfs :: Map String Module
        -> [(String, String, Signal)]
        -> (Map String Module, Int, Int)
    bfs mods [] = (mods, 0, 0)
    bfs mods ((_, _, Ignore):qs) = bfs mods qs
    bfs mods ((to, from, sig):qs) =
      if Map.notMember to mods
      then tallySignals $ bfs mods qs
      else let toMod = mods ! to
               (out, toMod') = processSignal from sig toMod
               mods' = Map.insert to toMod' mods
               q' = qs ++ map (, to, out) (children toMod)
      in traceChange toMod' $ tallySignals $ bfs mods' q'
      where
        tallySignals (ms, nLow, nHigh) =
          let dl = if sig == Low then 1 else 0
              dh = if sig == High then 1 else 0
          in (ms, nLow + dl, nHigh + dh)

        traceChange (Conjunction state _) = if to == "dg"
                                               && elem High (Map.elems state)
                                            then traceShow (n, state)
                                            else id
        traceChange _ = id


pushN :: Int -> Map String Module -> Int
pushN times ms = let (nLow, nHigh) = pushN' times ms
                   in nLow * nHigh
  where
    pushN' 0 _ = (0, 0)
    pushN' n mods = let (mods', dl, dh) = pushButton n mods
                        (nLow, nHigh) = pushN' (n-1) mods'
                    in (nLow + dl, nHigh + dh)

part1 :: IO ()
part1 = do
  mods <- parsed
  print $ pushN 1000 mods

part2 :: IO ()
part2 = do
  mods <- parsed
  print $ pushN 100000 mods
  -- Theoretically the biggest bruh moment of AoC:
  print $ lcm (96234 - 92467)
    $ lcm (96072 - 92143)
    $ lcm (96178 - 92355) (95950 - 91899)
