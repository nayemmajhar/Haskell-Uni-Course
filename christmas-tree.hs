{-
Implement a function christmasTree :: Int -> IO () that returns an IO action that outputs 
a Christmas Tree of given size (side length). For example, the executions of christmasTree 2, 
christmasTree 3, christmasTree 4 and christmasTree 5
-}
christmasTree :: Int -> IO ()
christmasTree n | n <= 2 = return () 
       			| otherwise = putStrLn ((topPart n) ++ (bottomPart n)) 


addSpace :: Int -> String
addSpace n = [' ' | y <- [1..n]]


addStar :: Int -> String
addStar n = ['*' | y <- [1..n]]


topPart :: Int -> String
topPart 0 = ""
topPart z = doIt z 0
           where
             doIt 0 x = ""
             doIt n x = (addSpace (n-1) ++ (addStar (x*2 + 1)) ++ "\n") ++ doIt (n-1) (x+1)

bottomPart :: Int -> String
bottomPart z = doIt (z-2)
             where
               doIt 0 = ""
               doIt n = ((addSpace (z-1)) ++ "*\n") ++ (doIt (n-1)) 







