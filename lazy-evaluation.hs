-- Prime Numbers
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p:[x | x <- xs, x `mod` p /= 0 ]

-- Fibonacci number -- 1
fibgen = [ n1 + n2 | (n1, n2) <-  zip (1: fibgen) (0:1:fibgen) ]

-- Fibonacci number --- 2
fiban :: Int -> Int -> [Int]
fiban n1 n2 = n1 : fiban n2 (n1+n2)

fibs :: [Int]
fibs = fiban 0 1

fib :: Int -> Int
fib n = fibs !! n

{-
In this task you should define a function allCombinations :: [a] -> [[a]], which calculates,
in ascending order, all combinations of the values given in the argument list. The generated list
of lists should be infinite.

ghci> take 7 (allCombinations [True,False])
[[],[True],[False],[True,True],[False,True],[True,False],[False,False]]
-}
allCombinations :: [a] -> [[a]]
allCombinations [] = [[]]
allCombinations xs = []:concat(map (\ys -> map (\x -> x:ys) xs)
                      (allCombinations xs))

{-
A number n is called an Avalanche number if there are non-negative integers i, j and k, 
so that n=3i∗5j∗7k. Define the (infinite) list avalanche :: [Integer], which, in Haskell,
sorts all Avalanche numbers in ascending order and returns them without duplicates.

ghci> take 10 avalanche
[1,3,5,7,9,15,21,25,27,35]
-}
avalanche :: [Integer]
avalanche =  1: map (3 *) avalanche
        `merge` map (5 *) avalanche
        `merge` map (7 *) avalanche

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ns = ns
merge ms [] = ms
merge (m:ms) (n:ns) = case compare m n of
    LT -> m : merge ms (n:ns)
    EQ -> m : merge ms ns
    GT -> n : merge (m:ms) ns

-- OptimizedPrimes
primesOptimized :: [Int]
primesOptimized = primeOps [2..]
                    where
                        primeOps (x:xs) = x : primeOps (filter (\y -> y `mod` x /= 0) xs