module Main where

import System.Environment (getArgs)

import Cube (Cube)
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

            case algorithm of
                "baseline" -> baselineSolution moves
                _ -> putStrLn $ "Unsupported algorithm: " ++ algorithm
        _ -> putStrLn "Usage: cabal run \"MOVE1 MOVE2 MOVE3 ..\" algorithm"





