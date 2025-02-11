module FizzBuzz where

ifThenElse :: Bool -> a -> a -> a
ifThenElse cond thenVal elseVal =
    case cond of
        True -> thenVal
        False -> elseVal    

fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz!"
    | n `mod` 3 == 0 = "Fizz!"
    | n `mod` 5 == 0 = "Buzz!"
    | otherwise = number n

lessThan20 :: Int -> String           
lessThan20 n
    | n > 0 && n < 20 = 
        let answers = words ("one two three four five six seven eight nine ten " ++
                        "eleven twelve thirteen fourteen fifteen sixteen " ++
                        "seventeen eighteen nineteen")
        in answers !! (n - 1) 

tens :: Int -> String
tens n 
    | n > 1 && n <= 9 =
        answers !! (n - 2)
    where
        answers = words "twenty thirty forty fifty sixty seventy eighty ninety"

number :: Int -> String
number n 
    | n > 0 && n < 20 = lessThan20 n
    | n `mod` 10 == 0 && n < 100 = tens (n `div` 10)
    | n < 100 = tens (n `div` 10) ++ " " ++ lessThan20 (n `mod` 10)
    | n == 100 = "one hundred"

main :: IO ()
main = do
    putStrLn "Ingrese un número entre 1 y 100:"
    input <- getLine
    let n = read input :: Int
    putStrLn (fizzbuzz n)
