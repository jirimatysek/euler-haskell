module Euler.ProjectEuler 
    where 

-- Core
fibSeq :: [Integer]
fibSeq = 1 : 1 : zipWith (+) fibSeq (tail fibSeq)

primeSeq :: [Integer]
primeSeq = primeSieve [2..]
    where
        primeSieve (x:xs) = x : [ y | y <- xs, y `mod` x /= 0] 

primeFactors :: Integer -> [Integer]
primeFactors n = primeFactorsInner n primeSeq
    where 
        primeFactorsInner x xs
            | x == (head xs)            = [x]
            | x `mod` (head xs) == 0    = (head xs) : primeFactorsInner (x `div` (head xs)) xs
            | otherwise                 = primeFactorsInner x (tail xs)

isPalindrome :: Integer -> Bool
isPalindrome n = n == reverseNumber n 0
    where
        reverseNumber n acc 
            | n == 0    = acc
            | otherwise = reverseNumber (n `div` 10) (10 * acc + n `mod` 10) 

-- Problem 1
-- Multiples of 3 and 5
problem001 :: Integer -> Integer
problem001 n = sumOfMultipleOf 3 + sumOfMultipleOf 5 - sumOfMultipleOf 15
    where 
        sumOfMultipleOf x = 
            let a = ((n - 1) `div` x)
            in x * ((a * (a + 1)) `div` 2)

-- Problem 2
-- Even Fibonacci numbers
problem002 :: Integer -> Integer
problem002 n = sum $ takeWhile (<n) $ filter even fibSeq

-- Problem 3
-- Largest prime factor
problem003 :: Integer -> Integer
problem003 n = maximum $ primeFactors n

-- Problem 4
-- Largest palindrome product
problem004 :: [Integer] -> Integer
problem004 xs = maximum $ filter isPalindrome [ x * y | x <- xs, y <- xs]





