
module FibTest (fib, fiblazy, sumlazy, euler, cyclic, cyclicNp) where

import           Data.Numbers.Primes

fib n = fib' n [1,1]

fib' n ac
    | n < 3 = ac
    | otherwise = fib' (n - 1) (ac ++ [s1 + s2])
    where s1 = last ac
          s2 = last $ init ac

fiblazy = 1:1:zipWith (+) fiblazy (tail fiblazy)

eul' 1 = 1
eul' n = length [x | x <- [1..n-1], 1 == gcd x n]

euler = [eul' n | n <- [1..]]

cyclic = [x | x <- [1..], 1 == gcd x (eul' x) ]

cyclicNp = [x | x <- cyclic, not $ isPrime x, x>1]

sum' n = sum [1..n]

sumlazy = [sum' n | n <- [1..]]
