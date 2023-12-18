module Day18 (part1, part2) where

import Lib
import Control.Applicative

type Instr = (Dir, Int)

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

toPosN :: Int -> Int -> Int -> Dir -> (Int, Int)
toPosN r c n dir = case dir of
                     UP -> (r-n, c)
                     DOWN -> (r+n, c)
                     LEFT -> (r, c-n)
                     RIGHT -> (r, c+n)

parsed :: Bool -> IO [Instr]
parsed isHex = do
  ls <- lines <$> fetchInput 2023 18 "day_18.txt"
  return $ map (execParser $ if isHex then hexP else instrP) ls
  where
    instrP = (,) <$> (dirP <* wsP)
             <*> (intP <* wsP)

    hexP = instrP *> stringP "(#" *> (toInstr <$> spanP (/= ')')) <* charP ')'

    toInstr xs = (digitToDir (last xs), parseHex (init xs))

    digitToDir x = case x of
                     '0' -> RIGHT
                     '1' -> DOWN
                     '2' -> LEFT
                     '3' -> UP
                     _ -> undefined

    dirP = (RIGHT <$ charP 'R')
           <|> (UP <$ charP 'U')
           <|> (DOWN <$ charP 'D')
           <|> (LEFT <$ charP 'L')
                                      


filled :: [Instr] -> Int
filled is = let a = abs $ area (vs ++ [head vs])
            in floor $ a + fromIntegral border / 2 + 1
  where
    (border, vs) = foldl toVert (0, [(0, 0)]) is

    toVert :: (Int, [(Int, Int)]) -> Instr -> (Int, [(Int, Int)])
    toVert (b, rest) (dir, n) = let (x, y) = head rest
                                    (y', x') = toPosN y x n dir
                                in (b + n, (x', y') : rest)
    
    area :: [(Int, Int)] -> Double
    area vs' = sum $ zipWith (\(x1, y1) (x2, y2) ->
                                0.5 * (fromIntegral y1 + fromIntegral y2)
                                * (fromIntegral x2 - fromIntegral x1)
                             ) vs' (tail vs')


part1 :: IO ()
part1 = do
  trench <- parsed False
  print $ filled trench

part2 :: IO ()
part2 = do
  trench <- parsed True
  print $ filled trench

