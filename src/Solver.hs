
module Solver where

import qualified Data.Vector as V
import Data.Foldable
import qualified System.Clock as Clock
import Data.IORef

import CubeColors


-- Check if there is cross in the middle matching middle points of the other sides
isCrossFront :: Cube -> Bool
isCrossFront c = all (== mm (front c)) [tm (front c), ml (front c), mr (front c), bm (front c)] && bm (top c) == mm (top c) 
    && mr(left c) == mm (left c) && tm (bottom c) == mm (bottom c) && ml (right c) == mm (right c)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

countTrues :: [Bool] -> Int
countTrues = sum . map boolToInt

countSameColorSide :: Side -> Int
countSameColorSide side = countTrues [tl side == mm side, tm side == mm side, tr side == mm side, ml side == mm side, mr side == mm side, bl side == mm side, bm side == mm side, br side == mm side]

countSameColor:: Cube -> Int
countSameColor cube = countSameColorSide (back cube) + countSameColorSide (top cube) + countSameColorSide (left cube) + countSameColorSide (front cube) + countSameColorSide (right cube) + countSameColorSide (bottom cube)

{-
Assigns score to the cube
-}
evaluate1 :: Cube -> Double
evaluate1 c 
    | otherwise = countCubeEntropy c

negativeInfinity :: Double
negativeInfinity = -1.0 / 0

selectBest :: [([String], Double, Int, Cube)] -> ([String], Double, Int, Cube)
selectBest states = maximumBy compareStates states
  where
    compareStates (_, score1, depth1, _) (_, score2, depth2, _) =
      compare score1 score2 <> compare depth2 depth1

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
                    return (moves, evaluate1 cube, depth, cube)
                else do
                -- Generate list of possible moves with corresponding labels
                let moveOptions = [
                        (rMove, "R"),
                        (lMove, "L"),
                        (uMove, "U"),
                        (dMove, "D"),
                        (bMove, "B"),
                        (fMove, "F"),
                        (prime rMove, "R'"),
                        (prime lMove, "L'"),
                        (prime uMove, "U'"),
                        (prime dMove, "D'"),
                        (prime bMove, "B'"),
                        (prime fMove, "F'"),
                        (\c -> rMove (rMove c), "R2"),
                        (\c -> lMove (lMove c), "L2"),
                        (\c -> uMove (uMove c), "U2"),
                        (\c -> dMove (dMove c), "D2"),
                        (\c -> bMove (bMove c), "B2"),
                        (\c -> fMove (fMove c), "F2")]
                results <- mapM (checkAndRunMove cube moves currentTime) moveOptions
                
                let !bestEval = selectBest  ((moves, evaluate1 cube, depth, cube) : results)
                -- putStrLn $ "Time: " ++ show (Clock.diffTimeSpec currentTime endTime)
                return bestEval
  where
    checkAndRunMove :: Cube -> [String] -> Clock.TimeSpec -> (Cube -> Cube, String) -> IO ([String], Double, Int, Cube)
    checkAndRunMove currentCube currentMoves startTime (moveFunc, moveNotation) = do
        currentTime <- Clock.getTime Clock.Monotonic
        -- putStrLn $ "Move: " ++ moveNotation ++ " Time: " ++ show (Clock.diffTimeSpec currentTime endTime)
        if currentTime >= endTime || ((length currentMoves) > 0 && (movesSameEdge (head currentMoves) moveNotation))
            then do
                return (currentMoves, negativeInfinity, depth, currentCube)
            else do
                let newCube = moveFunc currentCube
                let newMoves = moveNotation : currentMoves
                findMoves newCube (depth + 1) limit newMoves endTime

doNRandomMoves :: Cube -> [String] -> Int -> IO (Cube, [String])
doNRandomMoves cube moves 0 = return (cube, moves)
doNRandomMoves cube moves n = do
    randomMove <- getRandomMove
    let newCube = move randomMove cube
    let newMoves = (name randomMove) : moves
    doNRandomMoves newCube newMoves (n - 1)

solveUntilImprovement :: Cube -> [String] -> Double -> Clock.TimeSpec -> IO (Double, [String], Clock.TimeSpec)
solveUntilImprovement cube moves lastScore endTime = 
    do 
        let searchDepth = 3
        (bestMoves, score, _, newCube) <- findMoves cube 0 searchDepth moves endTime
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
                    --return (lastScore, moves, Clock.diffTimeSpec currentTime endTime)

                    (mixedCube, mixedMoves) <- doNRandomMoves cube moves searchDepth
                    (score3, moves3, _) <- solveUntilImprovement mixedCube mixedMoves (evaluate1 mixedCube) endTime
                    if score3 > lastScore
                        then do
                            return (score3, moves3, Clock.diffTimeSpec currentTime endTime)
                    else do
                        return (lastScore, moves, Clock.diffTimeSpec currentTime endTime)


addNanoSecs :: Clock.TimeSpec -> Integer -> Clock.TimeSpec
addNanoSecs (Clock.TimeSpec s ns) nsecs = Clock.TimeSpec s (ns + (fromIntegral nsecs))

baselineSolution :: Cube -> Integer -> IO ([String])
baselineSolution cube timeLimit = do
    startTime <- Clock.getTime Clock.Monotonic
    let endTime = addNanoSecs startTime (timeLimit * (10^(6::Integer)))
    (_, reversedMoves, _) <- solveUntilImprovement cube [] (evaluate1 cube) endTime
    let moves = reverse reversedMoves
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
