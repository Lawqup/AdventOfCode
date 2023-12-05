module Day5 (part1, part2) where

import Input
import Lib

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
                                        then dst + (prev - src)
                                        else apply prev es

apply prev [] = prev

convert :: [Map] -> Int -> Int
convert ms prev = foldl apply prev ms


part1 :: IO ()
part1 = do
  (seeds, maps) <- parsed intP
  print $ minimum $ map (convert maps) seeds


part2 :: IO ()
part2 = do
  (seeds, maps) <- parsed ((,) <$> (intP <* charP ' ') <*> intP)
  print $ minimum $ map (convert maps) $ seeds >>= (\(x,r) -> [x..x+r-1])

