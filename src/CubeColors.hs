{-
Rubik's cube is represented in the following way:
A tuple of tuples with fixed sizes, so data = (BACK, TOP, LEFT, FRONT, RIGHT, BOTTOM)
and BACK,TOP,..,BOTTOM = (TP, TM, TR, ML, MM, MR, BL, BM, BB) if we rotate to it from the front
    side via the shortest path (and to the back side we rotate through top side).

    IMPORTANT: CURRENTLY THE MIDDLE MOVE IS REVERSED (COMPARED TO STANDARD)
-}

module CubeColors where

import qualified Data.Vector as V
import Data.List (sort)
import System.Random

data Color = Yellow | Blue | Orange | White | Red | Green deriving (Enum, Show, Eq, Ord)

data Side = Side {
    tl :: Color, tm :: Color, tr :: Color,
    ml :: Color, mm :: Color, mr :: Color,
    bl :: Color, bm :: Color, br :: Color
} deriving Show

newtype ColorCubie = ColorCubie [Color] deriving (Show)

-- Check if they have the same colors
instance Eq ColorCubie where
    (ColorCubie c1) == (ColorCubie c2) = sort c1 == sort c2


toListSide :: Side -> [Color]
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


{-
Front to back from top left row major order
Each cubie colors are in this order: front, back, top, bottom, left, right
-}
getCubies :: Cube -> [ColorCubie]
getCubies (Cube bk tp lt fr rt bt) = 
    let
        cubieColors = [
            [tl fr, bl tp, tr lt],
            [tm fr, bm tp],
            [tr fr, br tp, tl rt],
            [ml fr, mr lt],
            [mr fr, ml rt],
            [bl fr, tl bt, br lt],
            [bm fr, tm bt],
            [br fr, tr bt, bl rt],
            [ml tp, tm lt],
            [mr tp, tm rt],
            [ml bt, bm lt],
            [mr bt, bm rt],
            [bl bk, tl tp, tl lt],
            [bm bk, tm tp],
            [br bk, tr tp, tr rt],
            [ml bk, ml lt],
            [mr bk, mr rt],
            [tl bk, bl bt, bl lt],
            [tm bk, bm bt],
            [tr bk, br bt, br rt]]
    in map ColorCubie cubieColors


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

solvedCube :: Cube
solvedCube = Cube {
    back = Side {tl = Yellow, tm = Yellow, tr = Yellow, ml = Yellow, mm = Yellow, mr = Yellow, bl = Yellow, bm = Yellow, br = Yellow},
    top = Side {tl = Blue, tm = Blue, tr = Blue, ml = Blue, mm = Blue, mr = Blue, bl = Blue, bm = Blue, br = Blue},
    left = Side {tl = Orange, tm = Orange, tr = Orange, ml = Orange, mm = Orange, mr = Orange, bl = Orange, bm = Orange, br = Orange},
    front = Side {tl = White, tm = White, tr = White, ml = White, mm = White, mr = White, bl = White, bm = White, br = White},
    right = Side {tl = Red, tm = Red, tr = Red, ml = Red, mm = Red, mr = Red, bl = Red, bm = Red, br = Red},
    bottom = Side {tl = Green, tm = Green, tr = Green, ml = Green, mm = Green, mr = Green, bl = Green, bm = Green, br = Green}
}

data Move = Move { 
    move :: Cube -> Cube,
    name :: String
}


possibleMoves :: [Move]
possibleMoves = [
    Move {move = rMove, name = "R"},
    Move {move = lMove, name = "L"},
    Move {move = uMove, name = "U"},
    Move {move = dMove, name = "D"},
    Move {move = bMove, name = "B"},
    Move {move = fMove, name = "F"},
    Move {move = (prime rMove), name = "R'"},
    Move {move = (prime lMove), name = "L'"},
    Move {move = (prime uMove), name = "U'"},
    Move {move = (prime dMove), name = "D'"},
    Move {move = (prime bMove), name = "B'"},
    Move {move = (prime fMove), name = "F'"},
    Move {move = (rMove . rMove), name = "R2"},
    Move {move = (lMove . lMove), name = "L2"},
    Move {move = (uMove . uMove), name = "U2"},
    Move {move = (dMove . dMove), name = "D2"},
    Move {move = (bMove . bMove), name = "B2"},
    Move {move = (fMove . fMove), name = "F2"}]

applyMove :: Cube -> String -> Cube
applyMove c moveName = moveFunction c
    where
        moveFunction = move $ head $ filter (\m -> name m == moveName) possibleMoves


getRandomMove :: IO Move
getRandomMove = do
    index <- randomRIO (0, length possibleMoves - 1)
    return $ possibleMoves !! index


applyMoves :: Cube -> [String] -> Cube
applyMoves c moves = foldl applyMove c moves

getCubeFromMoves :: [String] -> Cube
getCubeFromMoves moves = applyMoves solvedCube moves

{-
Add 1 to the element at index x in the vector
-}
addToList :: V.Vector Int -> Color -> V.Vector Int
addToList vec color = vec V.// [(cid, (vec V.! cid) + 1)]
    where
        cid = fromEnum color

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
