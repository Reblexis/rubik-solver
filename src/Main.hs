module Main where

import System.Environment (getArgs)

import Cube (getCubeFromMoves, applyMoves, evaluate)
import Solver (baselineSolution)


{- |

Arguments that have to be strictly passed to the program:
1. Shuffle that should be used to generate the initial state as a string of space separated tokens (strings) (e.g. "R L U D").
2. Algorithm that should be used as a string (e.g. "baseline").

Example call:
cabal run "R L U D" baseline

-}
main :: IO ()
main = do
    args <- getArgs
    case args of
        [shuffle, algorithm] -> do
            let moves = words shuffle  -- Split the shuffle into separate moves
            let cube = getCubeFromMoves moves

            solution <-
                    case algorithm of
                        "baseline" -> baselineSolution cube 1000
                        _ -> return []
            
            let finalCube = applyMoves cube solution
            let score = evaluate finalCube

            putStrLn $ "Solution: " ++ show solution
            putStrLn $ "Achieved score: " ++ show score
            
        _ -> putStrLn "Usage: cabal run \"MOVE1 MOVE2 MOVE3 ..\" algorithm"





