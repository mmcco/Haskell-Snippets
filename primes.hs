getPrimes :: Integer -> [Integer]
getPrimes n = primes n [2..]

primes :: Integer -> [Integer] -> [Integer]
primes 0 _    = []
primes n list = head list : primes (n-1) (sieve list)

sieve :: [Integer] -> [Integer]
sieve list = filter ((head list) `isntDivisorOf`) (tail list)

isntDivisorOf :: Integer -> Integer -> Bool
isntDivisorOf x y = y `mod` x /= 0

main =
    print $ getPrimes 10
