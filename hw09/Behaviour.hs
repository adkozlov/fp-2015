{-
Реализуйте runBehaviour
(1.5 балла)
-}

data Request  = Get           | Put String
data Response = Result String | OK

type Behaviour = [Response] -> [Request]

prog :: Behaviour
prog ~(OK : x : xs) = Put "more? " : Get : case x of
    Result "no" -> []
    Result "yes" -> prog xs
    _ -> Put "yes or no!" : prog (tail xs)

run :: Behaviour -> [Response] -> [Request] -> Int -> IO ()
run _ _ [] _ = return ()
run b r (Get:_) n = do
    l <- getLine
    runWithNew b (r ++ [Result l]) n
run b r ((Put s):_) n = do
    putStrLn s
    runWithNew b (r ++ [OK]) n

runWithNew :: Behaviour -> [Response] -> Int -> IO ()
runWithNew b r n = run b r (drop n $ b r) $ n + 1

runBehaviour :: Behaviour -> IO ()
runBehaviour b = run b [] (b []) 1

main :: IO ()
main = runBehaviour prog