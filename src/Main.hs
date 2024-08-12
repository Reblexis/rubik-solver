module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

import CubeColors (getCubeFromMoves, applyMoves, evaluate)
import CubeCubies (cubieFromColorCube)
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
            let colorCube = getCubeFromMoves moves
            let cubieCube = cubieFromColorCube colorCube

            solution <-
                    case algorithm of
                        "baseline" -> baselineSolution cubieCube 1000000
                        _ -> return []
            
            let finalCube = applyMoves colorCube solution
            let score = evaluate finalCube

            putStrLn $ "Solution: " ++ show solution
            putStrLn $ "Achieved score: " ++ show score
            hPutStrLn stderr $ "Cube: " ++ show finalCube

        _ -> putStrLn "Usage: cabal run \"MOVE1 MOVE2 MOVE3 ..\" algorithm"





