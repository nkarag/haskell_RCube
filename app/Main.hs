module Main where

import Rcube
import System.IO

main :: IO ()
main = do
    putStr "Give me Cube size: "
    --hSetBuffering  stdout  NoBuffering
    hFlush stdout
    n <- getLine
    let c = createSolvedCube $ read n
    putStrLn $ show c
    return ()

