module Parse
    ( Parser(..),
      execParser,
      maybeExecParser,
      spanP,
      spanCharP,
      charP,
      stringP,
      notNull,
      intP,
      digitP,
      sepBy,
      wsP,
      untilP,
    ) where

import Control.Applicative
import Data.Char (isDigit, isSpace, digitToInt)
import Data.Maybe (fromJust)
import Data.Functor

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

execParser :: Parser a -> String -> a
execParser p s = fromJust $ maybeExecParser p s

maybeExecParser :: Parser a -> String -> Maybe a
maybeExecParser p s = snd <$> runParser p s

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
                               (input', a) <- p input
                               Just (input', f a)


instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                                         (input', f) <- p1 input
                                         (input'', a) <- p2 input'
                                         Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
                     let (match, rest) = span f input
                     in Just (rest, match)

spanCharP :: (Char -> Bool) -> Parser Char
spanCharP fpred = Parser f
  where
    f (y:ys)
      | fpred y = Just (ys, y)
      | otherwise = Nothing
    f [] = Nothing

charP :: Char -> Parser Char
charP x = spanCharP (==x)

stringP :: String -> Parser String
stringP = traverse charP

-- Fails if value is null
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                              (input', xs) <- p input
                              if null xs
                                then Nothing
                                else Just (input', xs)

intP :: Parser Int
intP = sigP <*> (read <$> notNull (spanP isDigit))
  where
    sigP = (charP '-' $> (*(-1)) )
           <|> (charP '+' $> (*1) )
           <|> pure (*1)

digitP :: Parser Int
digitP = digitToInt <$> spanCharP isDigit

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

wsP :: Parser String
wsP = spanP isSpace

untilP :: Parser a -> Parser String
untilP afterP = afterP $> "" <|>
  ((:) <$> spanCharP (const True) <*> untilP afterP)
