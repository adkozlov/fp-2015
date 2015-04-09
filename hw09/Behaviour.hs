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

runBehaviour :: Behaviour -> IO ()
runBehaviour = undefined

main :: IO ()
main = runBehaviour prog
