import Data.List

numberOfDigits :: Int -> Int
numberOfDigits = length . show

lengthIsLessThan5 = (< 5) . length

repl :: Int -> a -> [a]
repl n = (take n) . repeat

repl' :: Int -> a -> [a]
repl' = (. repeat) . take

transposeString :: String -> String
transposeString = unlines . map unwords . transpose . map words . lines

mapn :: (Int -> a -> b) -> [a] -> [b]
mapn = undefined
