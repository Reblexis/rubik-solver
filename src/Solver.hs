
module Solver where

import qualified Data.Vector as V
import Data.Foldable
import qualified System.Clock as Clock
import Data.IORef
import System.IO (stderr, hPutStrLn)

import CubeCubies
import Data.Type.Equality (apply)


negativeInfinity :: Double
negativeInfinity = -(1.0 / 0)

selectBest :: [([String], Double, Int, Cube)] -> ([String], Double, Int, Cube)
selectBest = maximumBy compareStates
  where
    compareStates (_, score1, depth1, _) (_, score2, depth2, _) =
      compare score1 score2 <> compare depth2 depth1

evaluateCubie :: Cubie -> Int -> Int
evaluateCubie cubie pos 
    | position cubie == pos && rotation cubie == 0 = 3
    | position cubie == pos = 1
    | otherwise = 0

evaluate :: Cube -> Double
evaluate (Cube cubies) = fromIntegral(sum $ zipWith evaluateCubie cubies [0..19])

pruneMove :: String -> [String] -> Bool
pruneMove _ [] = False
pruneMove moveNotation moves
    | lastEdge == currentEdge = True
    | lastEdge == 'B' && currentEdge == 'F' = True
    | lastEdge == 'L' && currentEdge == 'R' = True
    | lastEdge == 'D' && currentEdge == 'U' = True
    | otherwise = False
    where
        lastMove = head moves
        lastEdge = head lastMove
        currentEdge = head moveNotation


{-# LANGUAGE BangPatterns #-}
findMoves :: Cube -> Int -> Int -> [String] -> Clock.TimeSpec -> IO ([String], Double, Int, Cube)
findMoves cube depth limit moves endTime = do
    currentTime <- Clock.getTime Clock.Monotonic
    --putStrLn $ "Depth: " ++ show depth ++ " Time: " ++ show (Clock.diffTimeSpec currentTime endTime)

    if currentTime >= endTime
        then do
            return (moves, negativeInfinity, depth, cube)
        else do
            if depth >= limit
                then do
                    return (moves, evaluate cube, depth, cube)
                else do

                results <- mapM (checkAndRunMove cube moves) possibleMoves

                let !bestEval = selectBest  ((moves, evaluate cube, depth, cube) : results)
                -- putStrLn $ "Time: " ++ show (Clock.diffTimeSpec currentTime endTime)
                return bestEval
  where
    checkAndRunMove :: Cube -> [String] -> NamedMove -> IO ([String], Double, Int, Cube)
    checkAndRunMove currentCube currentMoves (NamedMove mv moveNotation) = do
        currentTime <- Clock.getTime Clock.Monotonic
        -- putStrLn $ "Move: " ++ moveNotation ++ " Time: " ++ show (Clock.diffTimeSpec currentTime endTime)
        if currentTime >= endTime || pruneMove moveNotation currentMoves
            then do
                return (currentMoves, negativeInfinity, depth, currentCube)
            else do
                let newCube = applyMove currentCube mv
                let newMoves = moveNotation : currentMoves
                findMoves newCube (depth + 1) limit newMoves endTime


doNRandomMoves :: Cube -> [String] -> Int -> IO (Cube, [String])
doNRandomMoves cube moves 0 = return (cube, moves)
doNRandomMoves cube moves n = do
    randomMove <- getRandomMove
    let newCube = applyMove cube (move randomMove)
    let newMoves = name randomMove : moves
    doNRandomMoves newCube newMoves (n - 1)


solveUntilImprovement :: Cube -> [String] -> Double -> Clock.TimeSpec -> IO (Double, [String], Clock.TimeSpec)
solveUntilImprovement cube moves lastScore endTime =
    do
        let searchDepth = 6
        let currentDepth = length moves
        (bestMoves, score, _, newCube) <- findMoves cube currentDepth (searchDepth+currentDepth) moves endTime
        currentTime <- Clock.getTime Clock.Monotonic
        -- putStrLn $ "Final delay: " ++ show (Clock.diffTimeSpec currentTime endTime)
        if currentTime >= endTime
            then do
                return (lastScore, moves, Clock.diffTimeSpec currentTime endTime)
            else do
                if score > lastScore
                    then do
                        solveUntilImprovement newCube bestMoves score endTime
                else do
                    -- Write to stderr
                    --hPutStrLn stderr $ "Final score: " ++ show lastScore
                
                    --putStrLn $ "Final score: " ++ show lastScore
                    --return (lastScore, moves, Clock.diffTimeSpec currentTime endTime)
                    
                    (mixedCube, mixedMoves) <- doNRandomMoves cube moves searchDepth
                    (score3, moves3, _) <- solveUntilImprovement mixedCube mixedMoves (evaluate mixedCube) endTime
                    if score3 > lastScore
                        then do
                            return (score3, moves3, Clock.diffTimeSpec currentTime endTime)
                    else do
                        return (lastScore, moves, Clock.diffTimeSpec currentTime endTime)
                        


addNanoSecs :: Clock.TimeSpec -> Integer -> Clock.TimeSpec
addNanoSecs (Clock.TimeSpec s ns) nsecs = Clock.TimeSpec s (ns + fromIntegral nsecs)

baselineSolution :: Cube -> Integer -> IO [String]
baselineSolution cube timeLimit = do
    startTime <- Clock.getTime Clock.Monotonic
    let endTime = addNanoSecs startTime (timeLimit * (10^(6::Integer)))
    (score, reversedMoves, _) <- solveUntilImprovement cube [] (evaluate cube) endTime
    let moves = reverse reversedMoves
    hPutStrLn stderr $ "Final score: " ++ show score

    return moves


-- Basic test example: solveUntilImprovement (rMove$rMove$rMove solvedCube) [] 0

-- Current cube state, maximum depth, current depth, and a solution
{-
findSolutionIterative :: Cube -> Int -> Int -> ([String], Int)
findSolutionIterative cube maxDepth depth
    | depth > maxDepth = ([], -1)
    | otherwise = 
        let moves = findMoves cube depth []
        in if null moves then findSolutionIterative cube maxDepth (depth + 1) else moves

-}
