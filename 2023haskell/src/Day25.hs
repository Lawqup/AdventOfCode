module Day25 (genGraphviz, computeRes) where

import Lib

import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace

type Graph = Map String (Set String)

parsed :: IO Graph
parsed = do
  ls <- lines <$> fetchInput 2023 25 "day_25.txt"
  return $ addBackEdges $ Map.fromList $ map (execParser edgeP) ls
  where
    edgeP = do
      u <- spanP (/= ':')
      _ <- stringP ": "
      vs <- sepBy (charP ' ') (spanP (/= ' '))
      return (u, Set.fromList vs)

    addBackEdges :: Graph -> Graph
    addBackEdges m =
      foldl (\m' u ->
                let vs = m' ! u
                in foldl
                   (\m'' v -> let old = fromMaybe Set.empty $ m'' !? v
                                  new = Set.insert u old
                              in Map.insert v new m'')
                   m' vs)
      m
      $ Map.keys m

getCut :: Graph -> Maybe (Set String, Set String)
getCut g = let visited = dfs Set.empty $ fst $ Map.elemAt 0 g
           in if Map.size g - Set.size visited == 0
              then Nothing
              else Just (visited, Set.difference (Map.keysSet g) visited)
  where
    dfs :: Set String -> String -> Set String
    dfs visited u
      | Set.member u visited = visited
      | otherwise = let visited' = Set.insert u visited
                        vs = g ! u
                    in foldl dfs visited' vs

-- TODO use Stoer-Wagner
findCut :: Graph -> (Set String, Set String)
findCut g = tryCut edges3
  where
    ns = Map.keys g
    edges = [(u, v) | u <- ns, v <- ns, u < v, v `elem` (g ! u)]
    edges3 = [[e1, e2, e3] |
               e1 <- edges,
               e2 <- edges,
               e1 < e2,
               e3 <- edges,
               e2 < e3]

    tryCut [] = error "uh oh no more edges"
    tryCut (es:es3) = let g' = foldl (flip removeEdge) g es
                      in traceShow es $ case getCut g' of
                           Just cut -> cut
                           Nothing -> tryCut es3

removeEdge :: (String, String) -> Graph -> Graph
removeEdge (u, v) g' = let newu = Set.delete v $ g' ! u
                           newv = Set.delete u $ g' ! v
                       in Map.insert v newv $ Map.insert u newu g'
genGraphviz :: IO ()
genGraphviz = do
  graph <- parsed
  -- Run the file with dot -Tsvg -Kneato day25.dot > day25.svg
  let fp = "day25.dot"
  writeFile fp ""

  appendFile fp "graph { \n"
  mapM_ (\(u, vs) ->
           mapM_ (\v ->
                    appendFile fp $ "  " ++ u ++ " -- " ++ v ++ ";\n"
                 )
          vs)
    (Map.assocs graph)
  appendFile fp "}"


computeRes :: IO ()
computeRes = do
  g <- parsed
  let e1 = ("pcc", "htj")
      e2 = ("htb", "bbg")
      e3 = ("pjj", "dlk")
      g' = removeEdge e1 $ removeEdge e2 $ removeEdge e3 g
      (a, b) = fromJust $ getCut g'
  print $ Set.size a * Set.size b

