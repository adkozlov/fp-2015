--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
--import Control.Monad.State
--import Control.Monad.Error
--import Control.Monad.Identity

type Error = Either String -- Можно заменить на Maybe, если есть желание.
newtype Parser lex a = Parser { runParser :: [lex] -> Error (a, [lex]) }

--newtype Parser lex a = Parser { runParser :: StateT [lex] (ErrorT String Identity) a } deriving (Monad,Functor,Applicative,Alternative)

-- (0.5 балла)
evalParser :: Parser lex a -> [lex] -> Error a
evalParser p l = fst <$> runParser p l

-- (0.5 балла)
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy pred = Parser apply where
    apply [] = Left "list is empty"
    apply (x:xs) = case pred x of
        True -> Right (x, xs)
        False -> Left "lexeme doesn't satisfy"

-- (0.5 балла)
eof :: Parser lex ()
eof = Parser $ \ l -> case l of
    [] -> Right ((), [])
    otherwise -> Left "not eof"

--instance Functor (Parser lex) where
--    fmap f (Parser run) = Parser $ \ lexeme -> fmap (\ (g, h) -> (f g, h)) (run lexeme)

--instance Applicative (Parser lex) where
--    -- (0.5 балла)
--    pure x = Parser (\ s -> Right (x, s))
--    -- (1.5 балл)
--    (Parser run1) <*> (Parser run2) = Parser $ \lexeme -> case run1 lexeme of
--        (Left e) -> Left e
--        (Right (f, xs)) -> fmap (\ (g, h) -> (f g, h)) (run2 xs)

--instance Alternative (Parser lex) where
--    -- (0.5 балла)
--    empty = Parser $ \ _ -> Left "parser is empty"
--    -- (0.5 балла)
--    (Parser run1) <|> (Parser run2) = Parser $ \ lexeme -> case run1 lexeme of
--        (Left e) -> run2 lexeme
--        (Right (f, xs)) -> Right (f, xs)

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ null $ toList (p s)
