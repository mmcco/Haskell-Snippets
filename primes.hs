{-
    The primary function (getPrimes) returns a list of the
    first n primes using a Sieve of Eratosthenes via lazy
    evaluation.

    This was written as an example in a paper for a Discrete
    Math class. It therefore uses as little non-obvious syntax
    as possible. I may write another version using "where"
    syntax, type classes, and other such things.
-}

getPrimes :: Integer -> [Integer]
getPrimes n = primes n [2..]

primes :: Integer -> [Integer] -> [Integer]
primes 0 _    = []
primes n list = head list : primes (n-1) (sieve list)

sieve :: [Integer] -> [Integer]
sieve list = filter ((head list) `doesntDivide`) (tail list)

doesntDivide :: Integer -> Integer -> Bool
doesntDivide x y = y `mod` x /= 0

main =
    print $ getPrimes 10
