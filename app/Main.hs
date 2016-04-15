module Main where

import Rcube
import System.IO

prompt 
    :: Cube
    -> String -- prompt option
    -> IO()
prompt c "header" = do
    putStrLn ""
    putStrLn ""
    putStrLn "======================    cube    ======================"
    putStrLn $ show c
    putStrLn "========================================================"
    putStrLn ""
    putStrLn "**** Available Moves: U | D | L | R | F | B | M | E | S | U' | D' | L' | R' | F' | B' | M' | E' | S' | Ui | Di | Li | Ri | Fi | Bi | U'i | D'i | L'i | R'i | F'i | B'i, where i = 1,2,3,..."
    putStrLn "*  q - Quit"
    putStrLn "*  c - New Cube"
    putStrLn "*  ? - Is Cube Solved?"
    putStrLn "****"
    putStrLn ""
    putStrLn ""
    putStr "CUBE> "
    hFlush stdout
    command <- getLine
    interpret command c
prompt c "noheader" = do    
    putStr "CUBE> "
    hFlush stdout
    command <- getLine
    interpret command c

interpret
    :: String 
    -> Cube
    -> IO()
interpret [] c  = prompt c "noheader"
interpret "q" _ = do
    putStrLn "Bye and ... Happy Cubing!!!"
    hFlush stdout    
    return() 
interpret "c" _ = newCubeCommand    
interpret "?" c = if (isSolved c)
                    then do
                        putStrLn "Cube Solved!!!"
                        hFlush stdout
                        prompt c "noheader"
                    else do
                        putStrLn "Cube NOT Solved!!!"
                        hFlush stdout
                        prompt c "noheader"                                   
interpret moves c = do
    let a  = strToAlg moves 
    if a == [] 
        then prompt c "noheader"
    else
        do        
            let c' = listMove a c
            prompt c' "header"


newCubeCommand
    :: IO()
newCubeCommand = do
    -- Create a solved cube
    putStr "Give me Cube size: "
    --hSetBuffering  stdout  NoBuffering
    hFlush stdout
    n <- getLine
    let c = createSolvedCube $ read n
    prompt c "header"


main :: IO ()
main = do
    putStrLn "*********************************************************************"
    putStrLn "*                                                                   *"
    putStrLn "*                  <<<< SPEED CUBER >>>>>                           *"
    putStrLn "*                                                                   *"
    putStrLn "*********************************************************************"
    putStrLn ""    
    putStrLn "Copyright Nikos Karagiannidis (c) 2016"
    putStrLn "All rights reserved."
    putStrLn ""    
    hFlush stdout

    -- enter game loop
    newCubeCommand    
    
    -- main game loop
    --prompt c
{--
    -- get an alogorithm as input and execute it
    putStr "Give me an Algorithm: "
    --hSetBuffering  stdout  NoBuffering
    hFlush stdout
    a <- getLine
    let c' = listMove (strToAlg a) c
    putStrLn $ show c'
    
    return ()
--}
