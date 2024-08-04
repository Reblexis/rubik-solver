module Main where

import System.Environment (getArgs)

import Solver (findMoves, solveUntilImprovement)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Received parameters: " ++ show args