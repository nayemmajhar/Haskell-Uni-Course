{-
In this task you should implement the game Hangman in Haskell. This game is about
guessing a secret word in a certain number of attempts by guessing letters.

ghci> hangman "hallo"
Secret: *****
Enter a character: H
Secret: h****
Enter a character: l
Secret: h*ll*
-}


hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- hideGetLine
             putStrLn "Try to guess it: "
             guess word

hideGetLine :: IO String
hideGetLine = do x <- hideGetChar
                 if x == '\n' then
                    do putChar x
                        return []
                 else
                    do putChar '*'
                        xs <- hideGetLine
                        return (x:xs)

hideGetChar :: IO Char
hideGetChar = do hSetEcho stdin False
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


{- Hangman game with bind operations -}

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