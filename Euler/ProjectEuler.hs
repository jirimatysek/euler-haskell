module Euler.ProjectEuler 
    where

-- Problem 1
-- Multiples of 3 and 5
problem001 :: Integer -> Integer
problem001 n = sumOfN 3 + sumOfN 5 - sumOfN 15
    where 
        sumOfN x = 
            let a = ((n - 1) `div` x)
            in x * ((a * (a + 1)) `div` 2)

-- Problem 2
-- Even Fibonacci numbers
problem002 :: Integer -> Integer
problem002 n = sum $ takeWhile (<n) $ filter even fibSeq

fibSeq :: [Integer]
fibSeq = 1 : 1 : zipWith (+) fibSeq (tail fibSeq)
