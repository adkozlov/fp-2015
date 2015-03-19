import Data.Char
import Data.Either

findDigit :: String -> Maybe Int
findDigit [] = Nothing
findDigit (x:xs) = if (isDigit x) then (Just (digitToInt x)) else (findDigit xs)

findDigitAndIndex :: String -> Maybe (Int, Integer)
findDigitAndIndex [] = Nothing
findDigitAndIndex (x:xs) = findDigitAndIndex' (x:xs) 0 where
                           findDigitAndIndex' [] _ = Nothing
                           findDigitAndIndex' (x:xs) i | isDigit x = Just ((digitToInt x), i)
                                                       | otherwise = findDigitAndIndex' xs (i + 1)

findDigitAndIndexOrLen :: String -> Either (Int, Integer) Integer
findDigitAndIndexOrLen [] = Right 0
findDigitAndIndexOrLen (x:xs) = find' (x:xs) 0 where
                                find' [] len = Right len
                                find' (x:xs) i | isDigit x = Left ((digitToInt x), i)
                                               | otherwise = find' xs (i + 1)

data Result = DigitAndIndex Int Integer | LetterAndIndex Char Integer | Len Integer deriving Show

findDigitAndIndexOrLetterAndIndexOrLen :: String -> Result
findDigitAndIndexOrLetterAndIndexOrLen [] = Len 0
findDigitAndIndexOrLetterAndIndexOrLen (x:xs) = find' (x:xs) 0 where
                                                find' [] len = Len len
                                                find' (x:xs) i | isDigit x = DigitAndIndex (digitToInt x) i
                                                               | isLetter x = LetterAndIndex x i
                                                               | otherwise = find' xs (i + 1)

printResult :: Result -> String
printResult (DigitAndIndex d i) = "digit: " ++ show d ++ ", index: " ++ show i
printResult (LetterAndIndex l i) = "letter: " ++ show l ++ ", index: " ++ show i
printResult (Len l) = "length: " ++ show l