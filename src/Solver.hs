
-- Define the Side and Cube types
-- Define the Side and Cube types using records for clearer field access

import qualified Data.Vector as V
import Data.Foldable
import qualified System.Clock as Clock
import Data.IORef


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
Add 1 to the element at index x in the vector
-}
addToList :: V.Vector Int -> Int -> V.Vector Int
addToList v x = v V.// [(x, v V.! x + 1)]

countColors ::Side -> V.Vector Int
countColors side = foldl (addToList) (V.replicate 6 0) (toListSide side)

normalize :: V.Vector Int -> V.Vector Double
normalize counts = V.map(/ totalSum) vecDouble
    where
        totalSum = fromIntegral (V.sum counts) :: Double
        vecDouble = V.map fromIntegral counts

addLogProb :: Double -> Double -> Double
addLogProb acc x 
    | x == 0 = acc
    | otherwise = acc + x * log x

countEntropy :: V.Vector Double -> Double
countEntropy counts = (foldl (addLogProb) 0 counts)

countSideEntropy :: Side -> Double
countSideEntropy side = countEntropy (normalize (countColors side))

countCubeEntropy :: Cube -> Double
countCubeEntropy cube = sum (map countSideEntropy (toListCube cube))

{-
Assigns score to the cube
-}
evaluate :: Cube -> Double
evaluate c 
    | isCrossFront c = 
        let tlCorner = tl (front c) == mm (front c) && bl (top c) == bm (top c) && tr(left c) == mr(left c)
            trCorner = tr (front c) == mm (front c) && br (top c) == bm (top c) && tl(right c) == ml(right c)
            blCorner = bl (front c) == mm (front c) && tl (bottom c) == tm (bottom c) && br(left c) == mr(left c)
            brCorner = br (front c) == mm (front c) && tr (bottom c) == tm (bottom c) && bl(right c) == ml(right c)
        in 5.0+fromIntegral (countTrues [tlCorner, trCorner, blCorner, brCorner,
                        tlCorner && ml (top c) == mm (top c) && tm (left c) == mm(left c),
                        trCorner && mr (top c) == mm (top c) && tm (right c) == mm (right c),
                        blCorner && bm (left c) == mm (left c) && ml (bottom c) == mm (bottom c),
                        brCorner && bm (right c) == mm (right c) && mr (bottom c) == mm (bottom c)] + countSameColor c) + countCubeEntropy c

    | otherwise = fromIntegral(countTrues [tm (front c) == mm (front c) && bm (top c) == mm (top c), ml (front c) == mm (front c)&&mr(left c) == mm (left c),
                        mr (front c) == mm(front c) && ml (right c) == mm (right c), bm (front c) == mm (front c) &&tm (bottom c) == mm (bottom c)]) + countCubeEntropy c


evaluateMore:: Cube -> Double
evaluateMore c = max (evaluate c) (evaluate (xRotation c))

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
                    return (moves, evaluateMore cube, depth, cube)
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
                let !bestEval = selectBest results
                --putStrLn $ "Time: " ++ show (Clock.diffTimeSpec currentTime endTime)
                return bestEval
  where
    checkAndRunMove :: Cube -> [String] -> Clock.TimeSpec -> (Cube -> Cube, String) -> IO ([String], Double, Int, Cube)
    checkAndRunMove currentCube currentMoves startTime (moveFunc, moveNotation) = do
        currentTime <- Clock.getTime Clock.Monotonic
        --putStrLn $ "Move: " ++ moveNotation ++ " Time: " ++ show (Clock.diffTimeSpec currentTime endTime)
        if currentTime >= endTime
            then do
                return (currentMoves, negativeInfinity, depth, currentCube)
            else do
                let newCube = moveFunc currentCube
                let newMoves = currentMoves ++ [moveNotation]
                findMoves newCube (depth + 1) limit newMoves endTime

solveUntilImprovement :: Cube -> [String] -> Double -> Clock.TimeSpec -> IO (Double, [String], Clock.TimeSpec)
solveUntilImprovement cube moves lastScore endTime = 
    do 
        (bestMoves, score, _, _) <- findMoves cube 0 2 moves endTime
        currentTime <- Clock.getTime Clock.Monotonic
        putStrLn $ "Final delay: " ++ show (Clock.diffTimeSpec currentTime endTime)
        if score <= lastScore || currentTime >= endTime
            then return (lastScore, moves, Clock.diffTimeSpec currentTime endTime)
            else solveUntilImprovement cube bestMoves score endTime

addNanoSecs :: Clock.TimeSpec -> Integer -> Clock.TimeSpec
addNanoSecs (Clock.TimeSpec s ns) nsecs = Clock.TimeSpec s (ns + (fromIntegral nsecs))

baselineSolution :: Cube -> Integer -> IO (Double, [String], Clock.TimeSpec)
baselineSolution cube timeLimit = do
    startTime <- Clock.getTime Clock.Monotonic
    let endTime = addNanoSecs startTime (timeLimit * (10^(6::Integer)))
    solveUntilImprovement cube [] (negativeInfinity) endTime


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
