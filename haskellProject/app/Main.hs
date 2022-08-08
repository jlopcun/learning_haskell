module Main where

import Lib

--DO ACTIONS
doActions :: IO ()
doActions = do
    putStrLn "what is the base??"
    base <- getLine
    putStrLn "what is the height??"
    height <- getLine
    putStrLn ("the area of that triangle is: " ++ show ((read base * read height)/2))

--LEFT ARROW CLARIFICATIONS

leftArrow :: IO ()
leftArrow = do
    x <- putStrLn "what is your name??"
    name <- getLine
    print x


-- CONTROLLING ACTIONS
doGuessing :: (Ord t, Read t) => t -> IO ()
doGuessing num = do
    putStrLn "enter your number"
    guess <- getLine
    if read guess > num
        then do putStrLn "too high"
                doGuessing num
    else if read guess < num
        then do putStrLn "too low"
                doGuessing num
    else
        putStrLn "You win!!!!"



iKnowYou :: IO ()
iKnowYou = do
    putStrLn "What is your name?"
    name <- getLine
    if name == "Simon" || name == "Phil" || name == "John"
        then do putStrLn "I really think that haskell is a great programming language"
    else if name == "Koen"
        then do putStrLn "I think that debugging in haskell is fun!"
    else
        putStrLn "I do not know you, sorry :("

iKnowYouGuard :: IO ()
iKnowYouGuard = do
    putStrLn "What is your name?"
    name <- getLine
    let conditional name 
            |name == "Simon" || name == "Phil" || name == "John" = "I think haskell is a great programming language"
            |name == "Koen" = "I think that debugging in haskell is fun!"
            |otherwise = "I do not know you"
    putStrLn $ conditional name

main :: IO ()
main = do
    iKnowYouGuard



