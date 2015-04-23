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

symbol :: (Eq lex) => lex -> Parser lex ()
symbol x = () <$ satisfy (== x)

anySymbol :: Parser lex lex
anySymbol = satisfy (const True)

digit :: Parser Char Int
digit = (read . (\ c -> [c])) <$> satisfy isDigit

string :: (Eq lex) => [lex] -> Parser lex ()
string [] = pure ()
string (x:xs) = (symbol x) <* (string xs)

oneOf :: (Eq lex) => [lex] -> Parser lex lex
oneOf [] = empty
oneOf (x:xs) = satisfy (== x) <|> oneOf xs

many :: Parser lex a -> Parser lex [a]
many p = (many1 p <|> pure []) <|> pure []

many1 :: Parser lex a -> Parser lex [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

natural :: Parser Char Integer
natural = read <$> (many1 $ satisfy isDigit)

integer :: Parser Char Integer
integer = (negate <$> (symbol '-' *> natural)) <|> natural

spaces :: Parser Char ()
spaces = () <$ (many $ satisfy (== ' '))

try :: Parser lex a -> Parser lex (Maybe a)
try p = (Just <$> p) <|> pure Nothing

endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy pa pb = many (pa <* pb)

endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 pa pb = many1 (pa <* pb)

sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy pa pb = many $ (try pb <|> pure Nothing) *> pa

sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 pa pb = many1 $ (try pb <|> pure Nothing) *> pa

between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between pa pb pc = pa *> pc <* pb

brackets :: Parser Char a -> Parser Char a
brackets = between (symbol '[') (symbol ']')

parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')

braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')

angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')

foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P f pa pb = wrapper <$> pa <*> many ((,) <$> pb <*> pa) where
    wrapper s ((b, a):[]) = f s b a
    wrapper s ((b, a):xs) = f s b $ wrapper a xs

foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P f pa pb = wrapper <$> pa <*> many ((,) <$> pb <*> pa) where
    wrapper s ((b, a):[]) = f s b a
    wrapper s ((b, a):xs) = wrapper (f s b a) xs
