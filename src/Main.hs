module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

import CubeColors (getCubeFromMoves, applyMoves, evaluate)
import CubeCubies (cubieFromColorCube)
import Solver (findSolution)

{-| 
The main entry point for the Rubik's Cube solver program.

Expected command-line arguments:
1. Shuffle: A string of space-separated moves to generate the initial cube state.
2. Time limit: Maximum time allowed for solving (in seconds).
3. Search depth: Maximum depth for the search algorithm.
4. Search depth G1: Maximum depth for the G1 phase of the search.
5. Random moves number: Number of random moves to apply before solving.

Example usage:
@
cabal run "R LP U D" 30 20 12 5
@
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
            -- Uncomment the following line to print the final cube state
            -- hPutStrLn stderr $ "Cube: " ++ show finalCube

        _ -> putStrLn "Usage: cabal run \"MOVE1 MOVE2 MOVE3 ..\" <time_limit> <search_depth> <search_depth_g1> <random_moves_num>"