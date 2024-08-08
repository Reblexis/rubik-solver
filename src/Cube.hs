{-
Rubik's cube is represented in the following way:
A tuple of tuples with fixed sizes, so data = (BACK, TOP, LEFT, FRONT, RIGHT, BOTTOM)
and BACK,TOP,..,BOTTOM = (UL, UM, tr, ML, MM, MR, BL, BM, BB) if we rotate to it from the front
    side via the shortest path (and to the back side we rotate through top side).
    And colors are represented as integers via the following mapping: Y,B,O,W,R,G = 0,1,2,3,4,5


    IMPORTANT: CURRENTLY THE MIDDLE MOVE IS REVERSED (COMPARED TO STANDARD)
-}

module Cube where

import qualified Data.Vector as V

data Side = Side {
    tl :: Int, tm :: Int, tr :: Int,
    ml :: Int, mm :: Int, mr :: Int,
    bl :: Int, bm :: Int, br :: Int
} deriving Show

toListSide :: Side -> [Int]
toListSide (Side tl_ tm_ tr_ ml_ mm_ mr_ bl_ bm_ br_) = [tl_, tm_, tr_, ml_, mm_, mr_, bl_, bm_, br_]

data Cube = Cube {
    back :: Side,
    top :: Side,
    left :: Side,
    front :: Side,
    right :: Side,
    bottom :: Side
} deriving Show

toListCube :: Cube -> [Side]
toListCube (Cube back_ top_ left_ front_ right_ bottom_) = [back_, top_, left_, front_, right_, bottom_]

-- Function to rotate a Side clockwise
rotateSideC :: Side -> Side
rotateSideC s = Side {
    tl = bl s, tm = ml s, tr = tl s,
    ml = bm s, mm = mm s, mr = tm s,
    bl = br s, bm = mr s, br = tr s
}

-- Function to rotate a Side counterclockwise
rotateSideCC :: Side -> Side
rotateSideCC side = rotateSideC $ rotateSideC $ rotateSideC side

-- Function to modify the right column from another side's right column
changeRightFrom :: Side -> Side -> Side
changeRightFrom s1 s2 = Side {
    tl = tl s1, tm = tm s1, tr = tr s2,
    ml = ml s1, mm = mm s1, mr = mr s2,
    bl = bl s1, bm = bm s1, br = br s2
}

-- Function to perform an R move on the cube (rotates the right side and adjusts adjacent faces)
rMove :: Cube -> Cube
rMove c = Cube {
    back = changeRightFrom (back c) (top c),
    top = changeRightFrom (top c) (front c),
    left = left c,
    front = changeRightFrom (front c) (bottom c),
    right = rotateSideC (right c),
    bottom = changeRightFrom (bottom c) (back c)
}

-- Modify the middle column from another side's middle column
changeMiddleFrom :: Side -> Side -> Side
changeMiddleFrom s1 s2 = Side {
    tl = tl s1, tm = tm s2, tr = tr s1,
    ml = ml s1, mm = mm s2, mr = mr s1,
    bl = bl s1, bm = bm s2, br = br s1
}

-- Function to perform an M move on the cube (rotates the middle slice)
mMove :: Cube -> Cube
mMove c = Cube {
    back = changeMiddleFrom (back c) (top c),
    top = changeMiddleFrom (top c) (front c),
    left = left c,
    front = changeMiddleFrom (front c) (bottom c),
    right = right c,
    bottom = changeMiddleFrom (bottom c) (back c)
}

-- Function to rotate the entire cube clockwise
zRotation :: Cube -> Cube
zRotation c = Cube {
    back = rotateSideCC (back c),
    top = rotateSideC (left c),
    left = rotateSideC (bottom c),
    front = rotateSideC (front c),
    right = rotateSideC (top c),
    bottom = rotateSideC (right c)
}

-- Simplifying moves (unnecessary for the solver)
lMove :: Cube -> Cube
lMove c = zRotation $ zRotation $ rMove $ zRotation $ zRotation c

-- function to reverse a move by doing it 3 times
prime :: (Cube -> Cube) -> Cube -> Cube
prime f c = f $ f $ f c

uMove :: Cube -> Cube
uMove c = (prime zRotation) $ rMove $ zRotation c

dMove :: Cube -> Cube
dMove c = zRotation $ rMove $ (prime zRotation) c

xRotation :: Cube -> Cube
xRotation c = (prime lMove) $ rMove $ mMove c 

yRotation :: Cube -> Cube
yRotation c = zRotation$ rMove $ mMove $ (prime lMove) $ (prime zRotation) c

fMove :: Cube -> Cube
fMove c = (prime yRotation) $ rMove $ yRotation c

bMove :: Cube -> Cube
bMove c = (yRotation) $ rMove $ (prime yRotation) c

-- Check if a side is one color
isOneColor :: Side -> Bool
isOneColor s = all (== tl s) [tm s, tr s, ml s, mm s, mr s, bl s, bm s, br s]

-- Check if the entire cube is solved
isSolved :: Cube -> Bool
isSolved c = all isOneColor [back c, top c, left c, front c, right c, bottom c]

-- A representation of a solved cube
solvedCube :: Cube
solvedCube = Cube {
    back = Side {tl = 0, tm = 0, tr = 0, ml = 0, mm = 0, mr = 0, bl = 0, bm = 0, br = 0},
    top = Side {tl = 1, tm = 1, tr = 1, ml = 1, mm = 1, mr = 1, bl = 1, bm = 1, br = 1},
    left = Side {tl = 2, tm = 2, tr = 2, ml = 2, mm = 2, mr = 2, bl = 2, bm = 2, br = 2},
    front = Side {tl = 3, tm = 3, tr = 3, ml = 3, mm = 3, mr = 3, bl = 3, bm = 3, br = 3},
    right = Side {tl = 4, tm = 4, tr = 4, ml = 4, mm = 4, mr = 4, bl = 4, bm = 4, br = 4},
    bottom = Side {tl = 5, tm = 5, tr = 5, ml = 5, mm = 5, mr = 5, bl = 5, bm = 5, br = 5}
}

applyMove :: Cube -> String -> Cube
applyMove c move
    | move == "R" = rMove c
    | move == "L" = lMove c
    | move == "U" = uMove c
    | move == "D" = dMove c
    | move == "B" = bMove c
    | move == "F" = fMove c
    | move == "R'" = prime rMove c
    | move == "L'" = prime lMove c
    | move == "U'" = prime uMove c
    | move == "D'" = prime dMove c
    | move == "B'" = prime bMove c
    | move == "F'" = prime fMove c
    | otherwise = c

applyMoves :: Cube -> [String] -> Cube
applyMoves c moves = foldl applyMove c moves

getCubeFromMoves :: [String] -> Cube
getCubeFromMoves moves = applyMoves solvedCube moves

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

evaluate :: Cube -> Double
evaluate c = countCubeEntropy c
