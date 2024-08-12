module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

import CubeColors (getCubeFromMoves, applyMoves, evaluate)
import CubeCubies (cubieFromColorCube)
import Solver (findSolution)


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
        [shuffle, timeLimit, searchDepth, searchDepthG1, randomMovesNum] -> do
            let moves = words shuffle  -- Split the shuffle into separate moves
            let colorCube = getCubeFromMoves moves
            let cubieCube = cubieFromColorCube colorCube

            solution <- findSolution cubieCube (read timeLimit) (read searchDepth) (read searchDepthG1) (read randomMovesNum)
                                    
            let finalCube = applyMoves colorCube solution
            let score = evaluate finalCube

            putStrLn $ "Solution: " ++ show solution
            putStrLn $ "Achieved score: " ++ show score
            -- hPutStrLn stderr $ "Cube: " ++ show finalCube

        _ -> putStrLn "Usage: cabal run \"MOVE1 MOVE2 MOVE3 ..\" algorithm"





