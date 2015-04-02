-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTestOK
    , parserTestFail
    ) where

import Control.Applicative
import Test.HUnit
import Data.Foldable(toList)

type Error = Either String -- Можно заменить на Maybe, если есть желание.
newtype Parser lex a = Parser { runParser :: [lex] -> Error (a, [lex]) }

-- (0.5 балла)
evalParser :: Parser lex a -> [lex] -> Error a
evalParser = undefined

-- (0.5 балла)
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy = undefined

-- (0.5 балла)
eof :: Parser lex ()
eof = undefined

instance Functor (Parser lex) where
    fmap = undefined

instance Applicative (Parser lex) where
    -- (0.5 балла)
    pure = undefined
    -- (1.5 балл)
    (<*>) = undefined

instance Alternative (Parser lex) where
    -- (0.5 балла)
    empty = undefined
    -- (0.5 балла)
    (<|>) = undefined

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ null $ toList (p s)
