module Day5 (part1, part2) where

import Input
import Lib
import Debug.Trace

data Entry = Entry Int Int Int deriving Show
type Map = [Entry]

-- (seeds, maps)
parsed :: Parser a -> IO ([a], [Map])
parsed seedP = do
  inp <- fetchInput 2023 5 "day_5.txt"
  return $ execParser
    ((,) <$> (stringP "seeds: " *> sepBy spaceP seedP)
      <* wsP <*>
      sepBy (stringP "\n\n") mapP)
    inp
  where
    mapP = spanP (/= '\n') *> wsP *> sepBy (charP '\n')
      (Entry <$> intP <*> (spaceP *> intP) <*> (spaceP *> intP) )
    spaceP = charP ' '



apply :: Int -> Map -> Int
apply prev ((Entry dst src range):es) = if prev >= src && prev < src+range
                                        then dst + prev - src
                                        else apply prev es

apply prev [] = prev

applyRange :: (Int, Int) -> Map -> [(Int, Int)]
applyRange (prev, prevLen) ess@((Entry dst src range):es)
  -- src---prev---prev+len---src+range
  | prev >= src && prev+prevLen <= src+range = [(dst + prev - src, prevLen)]
  
  -- prev---src---prev+len---src+range
  | prev < src && prev+prevLen > src && prev+prevLen <= src + range
  = let r1 = (prev, src - prev)
        r2 = (src, prevLen - src + prev)
    in applyRange r1 ess ++ applyRange r2 ess

  -- prev---src---src+range---prev+len
  | prev < src && prev+prevLen > src+range
  = let r1 = (prev, src - prev)
        r2 = (src, range)
        r3 = (src+range, prevLen - range - src + prev)
    in applyRange r1 ess ++ applyRange r2 ess ++ applyRange r3 ess
    
  -- src---prev---src+range---prev+len
  | prev >= src && prev < src+range && prev+prevLen > src+range
  = let r1 = (prev, src + range - prev)
        r2 = (src+range, prev + prevLen - src - range)
    in applyRange r1 ess ++ applyRange r2 ess

  -- (prev, prevLen) completely outside of (src, range)
  | otherwise = applyRange (prev, prevLen) es

applyRange prevRange [] = [prevRange]


part1 :: IO ()
part1 = do
  (seeds, maps) <- parsed intP
  print $ minimum $ map (convert maps) seeds
  where
    convert ms prev = foldl apply prev ms

part2 :: IO ()
part2 = do
  (seeds, maps) <- parsed ((,) <$> (intP <* charP ' ') <*> intP)
  print $ minimum $ map (\x -> convert maps [x]) seeds
  where
    convert (m:ms) prev = convert ms $ prev >>= (\x -> let a = x `applyRange` m
                                                in traceShow a a)
    convert [] prev = minimum $ map fst prev

