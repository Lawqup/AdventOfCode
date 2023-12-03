module Day2 (part1, part2) where

import Input
import Lib
import Data.Maybe
import Control.Applicative

data Game = Game {gameId :: Int, gameDraws :: [(Int, Int, Int)] } deriving Show

-- Tuple is Red, Green, Blue
drawP :: Parser (Int, Int, Int)
drawP = foldl triadd (0,0,0) <$> sepBy (stringP ", ") colorP
  where
    colorP = toTuple <$> (intP <* charP ' ') <*> (stringP "red"
                                                  <|> stringP "green"
                                                  <|> stringP "blue")
    toTuple i color = case color of
                      "red" -> (i, 0, 0)
                      "green" -> (0, i, 0)
                      "blue" -> (0, 0, i)
                      _ -> undefined

    triadd (x1,x2,x3) (y1,y2,y3) = (x1+y1, x2+y2, x3+y3)

gameP :: Parser Game
gameP = Game <$> idP <*> drawsP
  where
    idP = stringP "Game " *> intP <* stringP ": "
    drawsP = sepBy (stringP "; ") drawP

parsed :: IO [Game]
parsed = do
  ls <- lines <$> fetchInput 2023 2 "day_2.txt"
  return (map (snd . fromJust . runParser gameP) ls)

part1 :: IO ()
part1 = do
  games <- parsed
  let valid =
        filter (all (\(r, g, b) ->
                        r <= 12 && g <= 13 && b <= 14)
                 . gameDraws) games
  print $ sum $ map gameId valid
  return ()

part2 :: IO ()
part2 = do
  draws <- map gameDraws <$> parsed
  let minCubes =
        map (foldl trimax (0,0,0)) draws
  print $ sum $ map power minCubes
  return ()
  
  where
    trimax (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)
    power (r,g,b) = r*g*b
