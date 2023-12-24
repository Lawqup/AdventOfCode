module Day24 (part1, part2) where

import Lib
import Data.List (intercalate)

-- x0, y0, z0, vx, vy, vz
type Path = (Double, Double, Double, Double, Double, Double)

parsed :: IO [Path]
parsed = do
  ls <- lines <$> fetchInput 2023 24 "day_24.txt"
  return $ map (execParser pathP) ls
  where
    pathP = do
      let numsP = map fromIntegral <$> sepBy (charP ',' *> wsP) intP
      is <- numsP
      _ <- wsP *> charP '@' *> wsP
      vs <- numsP
      
      return (head is, is !! 1, last is, head vs, vs !! 1, last vs)

collisions :: [Path] -> Int -> Int -> Int
collisions ps bound1 bound2 = length $ filter collides [(p1, p2) | p1 <- ps,
                                                         p2 <- ps,
                                                         p1 /= p2,
                                                         p1 < p2]
  where
    b1 = fromIntegral bound1
    b2 = fromIntegral bound2
    
    collides ((x1, y1, _, vx1, vy1, _), (x2, y2, _, vx2, vy2, _)) =
      let m1 = vy1 / vx1
          m2 = vy2 / vx2
          x = (m1 * x1 - m2 * x2 - y1 + y2) / (m1 - m2)
          y = m1 * (x - x1) + y1
          t1 = (x - x1) / vx1
          t2 = (x - x2) / vx2
      in t1 >= 0
         && t2 >= 0
         && b1 <= x && x <= b2
         && b1 <= y && y <= b2

part1 :: IO ()
part1 = do
  paths <- parsed
  let b1 = 200000000000000
      b2 = 400000000000000
  print $ collisions paths b1 b2

part2 :: IO ()
part2 = do
  paths <- parsed
  let fp = "day24.sage"
      nEqs = 3

  writeFile fp ""
  
  appendFile fp "var('x1', 'y1', 'z1', 'vx1', 'vy1', 'vz1')\n"

  let writeEqs i
        | i >= nEqs = return ()
        | otherwise =
          let (x2, y2, z2, vx2, vy2, vz2) = paths !! i
              ls = "var('t" ++ show i ++ "')\n"
              
                   ++ "eqX" ++ show i
                   ++ " = x1 + vx1 * t" ++ show i ++ " == "
                   ++ show x2 ++ " + " ++ show vx2 ++ " * t" ++ show i ++ "\n"
                   
                   ++ "eqY" ++ show i
                   ++ " = y1 + vy1 * t" ++ show i ++ " == "
                   ++ show y2 ++ " + " ++ show vy2 ++ " * t" ++ show i ++ "\n"
                   
                   ++ "eqZ" ++ show i
                   ++ " = z1 + vz1 * t" ++ show i ++ " == "
                   ++ show z2 ++ " + " ++ show vz2 ++ " * t" ++ show i ++ "\n"
 
          in appendFile fp ls *> writeEqs (i+1)
  writeEqs 0
  
  let eqsStr = intercalate ", "
        $ map (\i ->
                  "eqX" ++ show i ++ ", eqY" ++ show i ++ ", eqZ" ++ show i)
        [0..nEqs - 1]
      tStr = intercalate ", "
             $ map (("t" ++) . show) [0..nEqs - 1]

  appendFile fp $
    "S = solve([" ++ eqsStr ++ "], [x1, y1, z1, vx1, vy1, vz1, "
    ++ tStr ++ "])\n"
  -- print $ getStone paths
