{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- 1 балл
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
import Control.Monad.State
import Control.Monad.Error
import Test.HUnit((~?=),Test(TestCase),assertBool)

-- Реализуйте Parser, используя трансформеры монад.
newtype Parser lex a = Parser ? -- напишите сами
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runParser :: Parser lex a -> [lex] -> Either String (a, [lex])
runParser = undefined

evalParser :: Parser lex a -> [lex] -> Either String a
evalParser = undefined

satisfy :: (lex -> Bool) -> Parser lex lex
satisfy = undefined

eof :: Parser lex ()
eof = undefined

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK p s r = runParser p s ~?= Right r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail p s = TestCase $ assertBool "Parser should fail" $ either (const True) (const False) (runParser p s)
