module Day19 (part1, part2) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Lib
import Control.Applicative

type Part = (Int, Int, Int, Int)


data Condition = Ternary Char Char Int Condition Condition | Accepted Bool | Defer String

parsed :: IO (Map String Condition, [Part])
parsed = do
  inp <- fetchInput 2023 19 "day_19.txt"
  return $ execParser ((,)
                        <$> (execParser condsP <$> untilP (stringP "\n\n"))
                        <*> (stringP "\n\n" *> sepBy (charP '\n') partP)
                      ) inp
    where
      condsP = Map.fromList <$> sepBy (charP '\n') condP
      condP = (,) <$> untilP (charP '{') <* charP '{'
              <*> innerCondP <* charP '}'

      innerCondP = ternaryP <|> terminalP
      terminalP = (Accepted True <$ charP 'A')
                  <|> (Accepted False <$ charP 'R')
                  <|> (Defer <$> untilP (charP ',' <|> charP '}'))

      ternaryP = do
        cat <- charP 'x' <|> charP 'm' <|> charP 'a' <|> charP 's'
        op <- charP '<' <|> charP '>'
        val <- intP
        _ <- charP ':'
        r1 <- terminalP
        _ <- charP ','
        Ternary cat op val r1 <$> innerCondP

      partP :: Parser Part
      partP = charP '{' *>
              ((,,,) <$>
                (stringP "x=" *> intP <* charP ','))
              <*> (stringP "m=" *> intP <* charP ',')
              <*> (stringP "a=" *> intP <* charP ',')
              <*> (stringP "s=" *> intP)
              <* charP '}'


accepted :: Map String Condition -> Part -> Bool
accepted conds (x, m, a, s)= accepted' "in"
  where
    evalCond cond = case cond of
                      Accepted acc -> acc
                      Defer other -> accepted' other
                      Ternary cat op val r1 r2 -> evalTernary cat op val r1 r2

    evalTernary cat op val r1 r2 =
      let lhs = case cat of
                  'x' -> x
                  'm' -> m
                  'a' -> a
                  's' -> s
                  _ -> undefined
          cond = case op of
                   '<' -> (<)
                   '>' -> (>)
                   _ -> undefined
        in if cond lhs val then evalCond r1 else evalCond r2

    accepted' curr = evalCond (conds ! curr)



type Range = (Int, Int)
type Ranges = (Range, Range, Range, Range)

combinationsAccepted :: Map String Condition -> Int
combinationsAccepted conds = sum $ map combs $ splitOnWorkflow "in" vals
  where
    apply4 f (x, m, a, s) = (f x, f m, f a, f s)
    rangeSize (lo, hi) = hi - lo

    combs :: Ranges -> Int
    combs ranges = let (x, m, a, s) = apply4 rangeSize ranges
                   in x * m * a * s

    vals = ((1, 4001), (1, 4001), (1, 4001), (1, 4001))

    splitOnWorkflow :: String -> Ranges -> [Ranges]
    splitOnWorkflow curr = splitOnCond (conds ! curr)

    splitOnCond :: Condition -> Ranges -> [Ranges]
    splitOnCond cond ranges =
      case cond of
        Accepted acc -> [ranges | acc]
        Defer other -> splitOnWorkflow other ranges
        Ternary cat op val cond1 cond2 ->
          let (l, r) = splitRange cat op val ranges
          in applyCond cond1 l ++ applyCond cond2 r
          where
            applyCond _ Nothing = []
            applyCond cond' (Just ranges') = splitOnCond cond' ranges'

    splitRange :: Char -> Char -> Int -> Ranges -> (Maybe Ranges, Maybe Ranges)
    splitRange cat op val (x, m, a, s)
      | op == '<' = splitLess val
      | otherwise = let (l, r) = splitLess (val + 1)
                    in (r, l)
      where
        splitLess val'
          | lo < val' && hi > val' =
              (Just $ updRange (lo, val'), Just $ updRange (val', hi))
          | hi <= val' = (Just $ updRange (lo, hi), Nothing)
          | otherwise = (Nothing, Just $ updRange (lo, hi))
          
        updRange new = case cat of
                         'x' -> (new, m, a, s)
                         'm' -> (x, new, a, s)
                         'a' -> (x, m, new, s)
                         's' -> (x, m, a, new)
                         _ -> undefined

        (lo, hi) = case cat of
                     'x' -> x
                     'm' -> m
                     'a' -> a
                     's' -> s
                     _ -> undefined


part1 :: IO ()
part1 = do
  (conds, parts) <- parsed
  print $ sum $ map sumRating $ filter (accepted conds) parts
  where
    sumRating (x, m, a, s) = x + m + a + s

part2 :: IO ()
part2 = do
  (conds, _) <- parsed
  print $ combinationsAccepted conds
