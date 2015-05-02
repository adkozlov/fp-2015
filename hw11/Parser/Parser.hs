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
import Control.Monad.Identity
import Test.HUnit((~?=),Test(TestCase),assertBool)

-- Реализуйте Parser, используя трансформеры монад.
newtype Parser lex a = Parser { run :: StateT [lex] (ErrorT String Identity) a} -- напишите сами
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runParser :: Parser lex a -> [lex] -> Either String (a, [lex])
runParser p ls = runIdentity $ runErrorT $ runStateT (run p) ls

evalParser :: Parser lex a -> [lex] -> Either String a
evalParser p ls = case runParser p ls of
	Left e -> Left e
	Right (v, _) -> Right v

satisfy :: (lex -> Bool) -> Parser lex lex
satisfy p = Parser $ do
	ls <- get
	case ls of
		[] -> throwError "end of file"
		(l:ls') -> do
			if p l
			then do
				put ls'
				return l
			else do
				throwError "predicate is not true"

eof :: Parser lex ()
eof = Parser $ do
	ls <- get
	case ls of
		[] -> return ()
		_ -> throwError "not end of file"

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK p s r = runParser p s ~?= Right r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail p s = TestCase $ assertBool "Parser should fail" $ either (const True) (const False) (runParser p s)
