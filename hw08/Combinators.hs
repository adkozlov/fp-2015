module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- Поведение всех комбинаторов описано в тестах в Main.hs.

-- (0.5 балла)
symbol :: lex -> Parser lex ()
symbol = undefined

-- (0.5 балла)
anySymbol :: Parser lex lex
anySymbol = undefined

-- (0.5 балла)
digit :: Parser Char Int
digit = undefined

-- (0.5 балла)
string :: [lex] -> Parser lex ()
string = undefined

-- (0.5 балла)
oneOf :: [lex] -> Parser lex lex
oneOf = undefined

-- (0.5 балла)
many :: Parser lex a -> Parser lex [a]
many = undefined

-- (0.5 балла)
many1 :: Parser lex a -> Parser lex [a]
many1 = undefined

-- (0.5 балла)
natural :: Parser Char Integer
natural = undefined

-- (0.5 балла)
integer :: Parser Char Integer
integer = undefined

-- (0.5 балла)
spaces :: Parser Char ()
spaces = undefined

-- (0.5 балла)
try :: Parser lex a -> Parser lex (Maybe a)
try = undefined

-- (0.5 балла)
endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy = undefined

-- (0.5 балла)
endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 = undefined

-- (0.5 балла)
sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy = undefined

-- (0.5 балла)
sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 = undefined

-- (0.1 балла)
between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between = undefined

-- (0.1 балла)
brackets :: Parser lex a -> Parser lex a
brackets = undefined

-- (0.1 балла)
parens :: Parser lex a -> Parser lex a
parens = undefined

-- (0.1 балла)
braces :: Parser lex a -> Parser lex a
braces = undefined

-- (0.1 балла)
angles :: Parser lex a -> Parser lex a
angles = undefined

-- (1 балл)
foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P = undefined

-- (1 балл)
foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P = undefined
