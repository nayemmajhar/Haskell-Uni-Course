{-	
Implement a function diamond :: Int -> IO () that returns an IO action that outputs	
a diamond of given size (side length). For example, the executions of diamond 0, 	
diamond 1, diamond 2 and diamond 3	
-}

diamond :: Int -> IO ()
diamond n
    | n <= 0 = return ()
    | otherwise = putStr . unlines $ topStar n ++ middleStar n : bottomStar n

topStar n = map (addStar n) [1..n-1] 
middleStar n = addStar n n
bottomStar n = reverse (topStar n)

addStar :: Int -> Int -> String
addStar n i
    | i == 1 = (addSpace (n - i)) ++ "*"
    | otherwise = (addSpace (n - i)) ++ "*" ++ (addSpace (2*i - 3)) ++ "*" ++ (addSpace (n - i))

addSpace m = replicate m ' '


