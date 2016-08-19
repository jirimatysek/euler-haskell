module Euler.ProjectEuler 
    where



-- Problem 1
problem001 :: Integer -> Integer
problem001 n = sumOfN 3 + sumOfN 5 - sumOfN 15
    where 
        sumOfN x = 
            let a = ((n - 1) `div` x)
            in x * ((a * (a + 1)) `div` 2)

