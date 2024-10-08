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

-- | Represents the six possible colors on a Rubik's Cube
data Color = Yellow | Blue | Orange | White | Red | Green deriving (Enum, Show, Eq, Ord)

-- | Represents a single side of the Rubik's Cube
data Side = Side {
    tl :: Color, tm :: Color, tr :: Color,
    ml :: Color, mm :: Color, mr :: Color,
    bl :: Color, bm :: Color, br :: Color
} deriving Show

-- | Represents a single cubie (corner or edge piece) of the Rubik's Cube
newtype ColorCubie = ColorCubie [Color] deriving (Show)

-- | Equality for ColorCubie is defined by having the same colors, regardless of order
instance Eq ColorCubie where
    (ColorCubie c1) == (ColorCubie c2) = sort c1 == sort c2

-- | Convert a Side to a list of Colors
toListSide :: Side -> [Color]
toListSide (Side tl_ tm_ tr_ ml_ mm_ mr_ bl_ bm_ br_) = [tl_, tm_, tr_, ml_, mm_, mr_, bl_, bm_, br_]

-- | Represents the entire Rubik's Cube
data Cube = Cube {
    back :: Side,
    top :: Side,
    left :: Side,
    front :: Side,
    right :: Side,
    bottom :: Side
} deriving Show

-- | Convert a Cube to a list of Sides
toListCube :: Cube -> [Side]
toListCube (Cube back_ top_ left_ front_ right_ bottom_) = [back_, top_, left_, front_, right_, bottom_]

-- | Extract all cubies from the Cube
getCubies :: Cube -> [ColorCubie]
getCubies (Cube bk tp lt fr rt bt) = 
    let
        cubieColors = [
            [tl fr, bl tp, tr lt],
            [tl rt, br tp, tr fr],
            [br lt, tl bt, bl fr],
            [br fr, tr bt, bl rt],
            [tl lt, tl tp, bl bk],
            [br bk, tr tp, tr rt],
            [tl bk, bl bt, bl lt],
            [br rt, br bt, tr bk],
            [tm fr, bm tp],
            [ml fr, mr lt],
            [mr fr, ml rt],
            [bm fr, tm bt],
            [tm lt, ml tp],
            [tm rt, mr tp],
            [bm lt, ml bt],
            [bm rt, mr bt],
            [bm bk, tm tp],
            [ml bk, ml lt],
            [mr bk, mr rt],
            [tm bk, bm bt]]

    in map ColorCubie cubieColors

-- | Rotate a Side clockwise
rotateSideC :: Side -> Side
rotateSideC s = Side {
    tl = bl s, tm = ml s, tr = tl s,
    ml = bm s, mm = mm s, mr = tm s,
    bl = br s, bm = mr s, br = tr s
}

-- | Rotate a Side counterclockwise
rotateSideCC :: Side -> Side
rotateSideCC side = rotateSideC $ rotateSideC $ rotateSideC side

-- | Modify the right column of a Side from another Side's right column
changeRightFrom :: Side -> Side -> Side
changeRightFrom s1 s2 = Side {
    tl = tl s1, tm = tm s1, tr = tr s2,
    ml = ml s1, mm = mm s1, mr = mr s2,
    bl = bl s1, bm = bm s1, br = br s2
}

-- | Perform an R move on the cube (rotate right face clockwise)
rMove :: Cube -> Cube
rMove c = Cube {
    back = changeRightFrom (back c) (top c),
    top = changeRightFrom (top c) (front c),
    left = left c,
    front = changeRightFrom (front c) (bottom c),
    right = rotateSideC (right c),
    bottom = changeRightFrom (bottom c) (back c)
}

-- | Modify the middle column of a Side from another Side's middle column
changeMiddleFrom :: Side -> Side -> Side
changeMiddleFrom s1 s2 = Side {
    tl = tl s1, tm = tm s2, tr = tr s1,
    ml = ml s1, mm = mm s2, mr = mr s1,
    bl = bl s1, bm = bm s2, br = br s1
}

-- | Perform an M move on the cube (rotate middle slice)
mMove :: Cube -> Cube
mMove c = Cube {
    back = changeMiddleFrom (back c) (top c),
    top = changeMiddleFrom (top c) (front c),
    left = left c,
    front = changeMiddleFrom (front c) (bottom c),
    right = right c,
    bottom = changeMiddleFrom (bottom c) (back c)
}

-- | Rotate the entire cube clockwise around the Z-axis
zRotation :: Cube -> Cube
zRotation c = Cube {
    back = rotateSideCC (back c),
    top = rotateSideC (left c),
    left = rotateSideC (bottom c),
    front = rotateSideC (front c),
    right = rotateSideC (top c),
    bottom = rotateSideC (right c)
}

-- | Perform an L move on the cube (rotate left face clockwise)
lMove :: Cube -> Cube
lMove c = zRotation $ zRotation $ rMove $ zRotation $ zRotation c

-- | Reverse a move by applying it three times
prime :: (Cube -> Cube) -> Cube -> Cube
prime f c = f $ f $ f c

-- | Perform a U move on the cube (rotate top face clockwise)
uMove :: Cube -> Cube
uMove c = (prime zRotation) $ rMove $ zRotation c

-- | Perform a D move on the cube (rotate bottom face clockwise)
dMove :: Cube -> Cube
dMove c = zRotation $ rMove $ (prime zRotation) c

-- | Rotate the entire cube clockwise around the X-axis
xRotation :: Cube -> Cube
xRotation c = (prime lMove) $ rMove $ mMove c 

-- | Rotate the entire cube clockwise around the Y-axis
yRotation :: Cube -> Cube
yRotation c = zRotation$ rMove $ mMove $ (prime lMove) $ (prime zRotation) c

-- | Perform an F move on the cube (rotate front face clockwise)
fMove :: Cube -> Cube
fMove c = (prime yRotation) $ rMove $ yRotation c

-- | Perform a B move on the cube (rotate back face clockwise)
bMove :: Cube -> Cube
bMove c = (yRotation) $ rMove $ (prime yRotation) c

-- | Check if a Side is all one color
isOneColor :: Side -> Bool
isOneColor s = all (== tl s) [tm s, tr s, ml s, mm s, mr s, bl s, bm s, br s]

-- | Check if the entire Cube is solved
isSolved :: Cube -> Bool
isSolved c = all isOneColor [back c, top c, left c, front c, right c, bottom c]

-- | A solved Rubik's Cube
solvedCube :: Cube
solvedCube = Cube {
    back = Side {tl = Yellow, tm = Yellow, tr = Yellow, ml = Yellow, mm = Yellow, mr = Yellow, bl = Yellow, bm = Yellow, br = Yellow},
    top = Side {tl = Blue, tm = Blue, tr = Blue, ml = Blue, mm = Blue, mr = Blue, bl = Blue, bm = Blue, br = Blue},
    left = Side {tl = Orange, tm = Orange, tr = Orange, ml = Orange, mm = Orange, mr = Orange, bl = Orange, bm = Orange, br = Orange},
    front = Side {tl = White, tm = White, tr = White, ml = White, mm = White, mr = White, bl = White, bm = White, br = White},
    right = Side {tl = Red, tm = Red, tr = Red, ml = Red, mm = Red, mr = Red, bl = Red, bm = Red, br = Red},
    bottom = Side {tl = Green, tm = Green, tr = Green, ml = Green, mm = Green, mr = Green, bl = Green, bm = Green, br = Green}
}

-- | Represents a move on the Rubik's Cube
data Move = Move { 
    move :: Cube -> Cube,
    name :: String
}

-- | List of all possible moves on a Rubik's Cube
possibleMoves :: [Move]
possibleMoves = [
    Move {move = rMove, name = "R"},
    Move {move = lMove, name = "L"},
    Move {move = uMove, name = "U"},
    Move {move = dMove, name = "D"},
    Move {move = bMove, name = "B"},
    Move {move = fMove, name = "F"},
    Move {move = (prime rMove), name = "RP"},
    Move {move = (prime lMove), name = "LP"},
    Move {move = (prime uMove), name = "UP"},
    Move {move = (prime dMove), name = "DP"},
    Move {move = (prime bMove), name = "BP"},
    Move {move = (prime fMove), name = "FP"},
    Move {move = (rMove . rMove), name = "R2"},
    Move {move = (lMove . lMove), name = "L2"},
    Move {move = (uMove . uMove), name = "U2"},
    Move {move = (dMove . dMove), name = "D2"},
    Move {move = (bMove . bMove), name = "B2"},
    Move {move = (fMove . fMove), name = "F2"}]

-- | Apply a move to a Cube given the move name
applyMove :: Cube -> String -> Cube
applyMove c moveName = moveFunction c
    where
        moveFunction = move $ head $ filter (\m -> name m == moveName) possibleMoves

-- | Get a random move
getRandomMove :: IO Move
getRandomMove = do
    index <- randomRIO (0, length possibleMoves - 1)
    return $ possibleMoves !! index

-- | Apply a sequence of moves to a Cube
applyMoves :: Cube -> [String] -> Cube
applyMoves c moves = foldl applyMove c moves

-- | Get a Cube from a sequence of moves starting from a solved state
getCubeFromMoves :: [String] -> Cube
getCubeFromMoves moves = applyMoves solvedCube moves

-- | Increment the count for a specific color in a vector
addToList :: V.Vector Int -> Color -> V.Vector Int
addToList vec color = vec V.// [(cid, (vec V.! cid) + 1)]
    where
        cid = fromEnum color

-- | Count the occurrences of each color on a Side
countColors ::Side -> V.Vector Int
countColors side = foldl (addToList) (V.replicate 6 0) (toListSide side)

-- | Normalize a vector of counts to probabilities
normalize :: V.Vector Int -> V.Vector Double
normalize counts = V.map(/ totalSum) vecDouble
    where
        totalSum = fromIntegral (V.sum counts) :: Double
        vecDouble = V.map fromIntegral counts

-- | Helper function for entropy calculation
addLogProb :: Double -> Double -> Double
addLogProb acc x 
    | x == 0 = acc
    | otherwise = acc + x * log x

-- | Calculate the entropy of a normalized vector
countEntropy :: V.Vector Double -> Double
countEntropy counts = (foldl (addLogProb) 0 counts)

-- | Calculate the entropy of a Side
countSideEntropy :: Side -> Double
countSideEntropy side = countEntropy (normalize (countColors side))

-- | Calculate the total entropy of a Cube
countCubeEntropy :: Cube -> Double
countCubeEntropy cube = sum (map countSideEntropy (toListCube cube))

-- | Evaluate the "scrambledness" of a Cube based on its entropy
evaluate :: Cube -> Double
evaluate c = countCubeEntropy c
