import Data.Char (toUpper)
import Data.List (sort)

{-
Each comprehension should have the exact form [ ... | i <- [1 .. 5], ...]
-}
-- yields [[3],[5],[7],[9],[11]]
oddNumList :: [[Int]]
oddNumList = [[i * 2 + 1] | i <- [1..5]]

-- yields [(5,False),(20,True),(25,False)]
oddNumBool :: [(Int,Bool)]
oddNumBool = [(5 * i, i == 4)| i <- [1..5], i == 1 || i > 3]

-- yields [Just 1,Just 9,Just 25]
maybeList :: [Maybe Integer]
maybeList = [Just x | i <- [1..5], odd i, let x = if i == 1 
                                              then i
                                              else i*3
            ]

maybeList2 :: [Maybe Integer]  
maybeList2 = [Just (i*i)| i <- [1..5], odd i]

-- yields [(1,5),(1,4),(1,3),(1,2),(2,5),(2,4),(2,3),(3,5),(3,4),(4,5)]
listTuple :: [(Int, Int)]
listTuple = [ (i,j) | i <- [1..5], j <- [5,4..i+1] ]

-- listLookup :: Eq a => a -> [(a,b)] -> Maybe b
listLookup i cp = checkJust [ y | (x,y) <- cp, i == x]
    where checkJust [] = Nothing
          checkJust [x] = Just x

-- replicate an character
listReplicate :: Int -> a -> [a]
listReplicate i x = [ x | _ <- [1..i]]

{- Pythagoras theory -}
pithList n = [(x,y,z)| x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

pythagorean n = [(x, y, z) |  x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]

-- uppercase all item of a list
listToUpper xs = [toUpper c | c <-xs]

-- nested list comprehensionne
nestedList = [[(i,j) | i <- [1,2]] | j <- [1..]]

-- perfect numbers and factors
factors' n = [x | x <- [1..n-1], n `mod` x == 0]
perfects n = [x | x <- [1..n], sum (factors' x) == x]

-- Count lenght of a list
count' x xs = length [ l | l <- xs, l == x]

{- find item in tuples -}
find k t = [j | (i,j) <- t, i == k]

-- make double of all positive number
doublePos :: [Int] -> [Int]
doublePos xs = [2*x | x <- xs, x > 0]

-- Repeat a Item
spaces :: Int -> String
spaces n = [ 'N' | i <- [1..n] ]

-- Change coins/money
type Coin = Int
coins :: [Coin]
coins = reverse (sort [1,2,5,10,20,50,100,500])

allChange :: Int -> [[Coin]]
allChange 0 = [[]]
allChange amount = [ c:cs |
        c <- coins, amount >= c,
        cs <- allChange ( amount - c)]

change amount = head ( allChange amount )

-- list of square of even number 
gensquares :: Int -> Int -> [Int]
gensquares min max = [ x^2 | x <- [min..max], even x]

listCompps = [(x,y) | x <- [1,3,5], y <- [2,4,6], x<y]

-- filter function with list comprehension
listFilter f xs = [x | x <- xs, f x]

-- map function with list comprehension
listMap f xs = [f x | x <- xs]
