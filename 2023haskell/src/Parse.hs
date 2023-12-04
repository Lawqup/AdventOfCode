module Parse
    ( Parser(..), spanP, charP, stringP, notNull, intP, sepBy, execParser, wsP
    ) where

import Control.Applicative
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

execParser :: Parser a -> String -> a
execParser p s = snd $ fromJust $ runParser p s

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

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | x == y = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

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
intP = f <$> notNull (spanP isDigit)
  where
    f = read

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

wsP :: Parser String
wsP = spanP isSpace
