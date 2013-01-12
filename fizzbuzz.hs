module Main where

main = mapM_ (putStrLn . fizzBuzz) [1..100]

fizzBuzz num
	| num `mod` 15 == 0 = "FizzBuzz"
	| num `mod` 3 == 0 = "Fizz"
	| num `mod` 5 == 0 = "Buzz"
	| otherwise = show num
