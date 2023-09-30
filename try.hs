
-- Function to check if a number is even
even' :: Int -> Bool
even' n = n `mod` 2 == 0

-- Function to check if a number is odd using the even' function
odd' :: Int -> Bool
odd' n = not (even' n)

-- Function to calculate the nth Fibonacci number
computeFibonacci :: Int -> Int
computeFibonacci n
    | n <= 0 = error "Incorrect input"
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = computeFibonacciHelper 1 1 (n - 2)

computeFibonacciHelper :: Int -> Int -> Int -> Int
computeFibonacciHelper a b 0 = b
computeFibonacciHelper a b n = computeFibonacciHelper b (a + b) (n - 1)

-- Function to calculate the sum of odd Fibonacci numbers up to the nth number
sumOddFibonacci :: Int -> Int
sumOddFibonacci n = sum [x | x <- take n fibs, odd' x]
    where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Function to compute the sum of digits of a number
sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)

-- Function to compute the magic number for a given input
computeMagicNumber :: Int -> Int
computeMagicNumber n
    | n < 10 = n
    | otherwise = computeMagicNumber (sumDigits n)

