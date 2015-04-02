import Data.Functor
import Control.Applicative

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = (:) <$> p <*> (((const id) <$> sep <*> sepBy p sep) <|> pure [])

type Error = String
newtype Parser lexeme a = Parser { run::[lexeme] -> Either Error (a, [lexeme]) }

instance Functor (Parser lexeme) where
--  fmap g (Parser run) = Parser $ \lexeme -> case run lexeme of
--    (Left e) -> Left e
--    (Right (a, l)) -> Right (g a, l)
  fmap f (Parser run) = Parser $ \lexeme -> fmap (\ (g, h) -> (f g, h)) (run lexeme)

instance Applicative (Parser lexeme) where
  pure x = Parser (\ s -> Right (x, s))
  (Parser run1) <*> (Parser run2) = Parser $ \lexeme -> case run1 lexeme of
    (Left e) -> Left e
    (Right (f, xs)) -> fmap (\ (g, h) -> (f g, h)) (run2 xs)

instance Alternative (Parser lexeme) where
  empty = Parser $ \ lexeme -> Left "parser is empty"
  (Parser run1) <|> (Parser run2) = Parser $ \ lexeme -> case run1 lexeme of
    (Left e) -> run2 lexeme
    (Right (f, xs)) -> Right (f, xs)
