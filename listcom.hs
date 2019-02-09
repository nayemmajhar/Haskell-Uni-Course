import System.IO

{- A sequence of actions can be combines as a single composite
   action using the keyword __do__
-}
haskIO :: IO (Char,Char)
haskIO = do x <- getChar
            y <- getChar
            return (x,y)

{- ** getLine is a library function **
   READING a string from Keyboard
-}
getLines :: IO String
getLines = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLines
                   return (x:xs)

{- ** putStr is a library function **
   WRITING a string to screen
-}
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

{- putStrLn is a library function
   WRITING a string and Moving to a new line
-}
putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

{- We can define an action that prompts for a 
   sting to be entered and displays its length
-}

strLength :: IO ()
strLength = do putStr "Enter a string: "
               xs <- getLine
               putStr "The String has "
               putStr (show (length xs))
               putStrLn " characters"


{- HANGMAN Game
   1. 1st player enter a word
   2. 2nd player try to guess the word
   3. If guess is correct then game ends
      otherwise continue
-}

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '*'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                    putStrLn "You got it!"
                else
                    do putStrLn (diff word xs)
                       guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '*' | x <- xs]

getNonEmptyLine :: IO String
getNonEmptyLine =
    getLine >>= \input ->
    if null input
        then putStr "Please enter a non-empty string." >> getNonEmptyLine
        else return input >>= \result ->
    return result

main = do
    putStrLn "What's your first name? "
    firstName <- getLine
    putStrLn "What's your last name? "
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", How are you?"


readNonEmpty :: IO String
readNonEmpty = do
    str      <- getLine
    if null str
    then do putStrLn "Please insert a non-empty string"
            readNonEmpty
    else return str

readNonEmptyDo :: IO String
readNonEmptyDo = getLine >>= \str ->
                    if null str
                    then putStrLn "Please insert a non-empty string" >> readNonEmpty
                    else return str >>= \nonEmpty ->
                return nonEmpty

{- SEQUENCING -}
{- A sequence of actions can be combined as a single composite action
   uning the keyword do -}

sequence' :: IO (Char, Char)
sequence' = do x <- getChar
                y <- getChar
                return (x,y)

{- Convert sequence' to bind notation -}

sequenceBind :: IO (Char, Char)
sequenceBind = getChar >>= \x ->
                getChar >>= \y ->
                return (x,y)

getLine' :: IO String
getLine' = do x <- getChar
                if x == '\n' then
                return []
                else 
                    do xs <- getLine'
                        return (x:xs)

{- Convert getLine' to bind notation -}

getLineBind = getChar >>= \x ->
                if x == '\n' then
                return []
                else
                getLineBind >>= \xs ->
                return (x:xs)

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The line has "
            putStr (show(length xs))
            putStr " characters.\n"

{- Using return doesn't cause the I/O do block to end in execution
or anything like that  -}
midNoReturn = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line 

ioActions :: [IO ()]
ioActions = [(print "Hello!"),
                (putStr "just kidding"),
                (getChar >> return ())
            ]

sequence2 :: [IO a] -> IO ()
sequence2 [] = return ()
sequence2 (x:xs) = do x
                        sequence_ xs

addIO :: Num a => a -> IO a
addIO n = return (n + 1)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO actFun []     = return []
mapIO actFun (x:xs) = do
    x <- actFun x
    xs <- mapIO actFun xs
    return (x:xs)

-- filterIO :: (a -> Bool) -> [a] -> [a]

facL :: Int -> Int
facL 0 = 1
facL n = n * facL (n-1)

facIO :: Int -> IO Int
facIO 0 = do
    return 1

facIO n = do
    facNx <- facIO (n-1)
    let nx = n * facNx
    return nx

facKey = do
    str <- getLine
    let n = read str
    fac <- facIO n
    return fac


{-
Main> game
Think of a number between 1 and 100!
Is it 50? higher
Is it 75? lower
Is it 62? lower
Is it 56? yes
Great, I won!
-}
numberGame :: IO ()
numberGame = do
    putStr "Think of a number between 1 and 100!\n"
    num <- getLineHide
    guessNumber num

getLineHide :: IO String
getLineHide = do
    x <- getCharHide
    if x == '\n' then
        do putChar x
           return []
    else do
            putChar '*'
            xs <- getLineHide
            return (x:xs)

getCharHide :: IO Char            
getCharHide =  do hSetEcho stdin False
                  c <- getChar
                  hSetEcho stdin True
                  return c


guessNumber :: String -> IO ()
guessNumber s = do
    putStr "Is it "
    x <- getLine
    if x == s then
        putStr "Great, I won!\n"
    else do
        print (checkNumber x s)
        guessNumber s


checkNumber :: Ord a => a -> a -> String
checkNumber x s = if x > s then "Lower" else "Higher"

{- HANGMAN Game
   1. 1st player enter a word
   2. 2nd player try to guess the word
   3. If guess is correct then game ends
      otherwise continue
-}
{- Hangman in Bind -}

hangmanBind :: IO ()
hangmanBind = 
    putStrLn "Think of a word: " >> sgetLineBind >>= \word ->
    putStrLn "Try to guess it: " >> guessBind word

sgetLineBind :: IO [Char]
sgetLineBind =
    getCharBind >>= \x ->
    if x == '\n' then
        putChar x >> return []
    else
        putChar '*' >> sgetLineBind >>= \xs ->
        return (x:xs)

getCharBind :: IO Char
getCharBind = 
    hSetEcho stdin False >> getChar >>= \c ->
    hSetEcho stdin True >> return c

guessBind :: String -> IO ()
guessBind word = 
    putStr "> " >>
    getLine >>= \xs ->
    if xs == word then
        putStrLn "You got it!"
    else
        putStrLn (diffBind word xs) >> guessBind word