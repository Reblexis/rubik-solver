{-
Rubik's cube is represented in the following way:
A tuple of tuples with fixed sizes, so data = (BACK, TOP, LEFT, FRONT, RIGHT, BOTTOM)
and BACK,TOP,..,BOTTOM = (UL, UM, UR, ML, MM, MR, BL, BM, BB) if we rotate to it from the front
    side via the shortest path (and to the back side we rotate through top side).
    And colors are represented as integers via the following mapping: Y,B,O,W,R,G = 0,1,2,3,4,5


    IMPORTANT: CURRENTLY THE MIDDLE MOVE IS REVERSED (COMPARED TO STANDARD)
-}
-- Define the Side and Cube types
-- Define the Side and Cube types using records for clearer field access
data Side = Side {
    ul :: Int, um :: Int, ur :: Int,
    ml :: Int, mm :: Int, mr :: Int,
    bl :: Int, bm :: Int, br :: Int
} deriving Show

data Cube = Cube {
    back :: Side,
    top :: Side,
    left :: Side,
    front :: Side,
    right :: Side,
    bottom :: Side
} deriving Show

-- Function to rotate a Side clockwise
rotateSideC :: Side -> Side
rotateSideC s = Side {
    ul = bl s, um = ml s, ur = ul s,
    ml = bm s, mm = mm s, mr = um s,
    bl = br s, bm = mr s, br = ur s
}

-- Function to rotate a Side counterclockwise
rotateSideCC :: Side -> Side
rotateSideCC side = rotateSideC $ rotateSideC $ rotateSideC side

-- Function to modify the right column from another side's right column
changeRightFrom :: Side -> Side -> Side
changeRightFrom s1 s2 = Side {
    ul = ul s1, um = um s1, ur = ur s2,
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
    ul = ul s1, um = um s2, ur = ur s1,
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


-- Check if there is cross in the middle matching middle points of the other sides
isCross :: Cube -> Bool
isCross c = all (== mm (front c)) [um (front c), ml (front c), mr (front c), bm (front c)] && bm (top c) == mm (top c) 
    && mr(left c) == mm (left c) && um (bottom c) == mm (bottom c) && ml (right c) == mm (right c)

-- Check if a side is one color
isOneColor :: Side -> Bool
isOneColor s = all (== ul s) [um s, ur s, ml s, mm s, mr s, bl s, bm s, br s]

-- Check if the entire cube is solved
isSolved :: Cube -> Bool
isSolved c = all isOneColor [back c, top c, left c, front c, right c, bottom c]

-- A representation of a solved cube
solvedCube :: Cube
solvedCube = Cube {
    back = Side {ul = 0, um = 0, ur = 0, ml = 0, mm = 0, mr = 0, bl = 0, bm = 0, br = 0},
    top = Side {ul = 1, um = 1, ur = 1, ml = 1, mm = 1, mr = 1, bl = 1, bm = 1, br = 1},
    left = Side {ul = 2, um = 2, ur = 2, ml = 2, mm = 2, mr = 2, bl = 2, bm = 2, br = 2},
    front = Side {ul = 3, um = 3, ur = 3, ml = 3, mm = 3, mr = 3, bl = 3, bm = 3, br = 3},
    right = Side {ul = 4, um = 4, ur = 4, ml = 4, mm = 4, mr = 4, bl = 4, bm = 4, br = 4},
    bottom = Side {ul = 5, um = 5, ur = 5, ml = 5, mm = 5, mr = 5, bl = 5, bm = 5, br = 5}
}

-- Current cube state, remaining depth, accumulated moves, and a solution
findMoves :: Cube -> Int -> [String] -> [String]
findMoves cube 0 moves 
    | isCross cube = moves
    | otherwise = []
findMoves cube depth moves
    | isCross cube = moves
    | otherwise = 
        let r = findMoves (rMove cube) (depth - 1) (moves ++ ["R"])
            m = findMoves (mMove cube) (depth - 1) (moves ++ ["M"])
            c = findMoves (zRotation cube) (depth - 1) (moves ++ ["RC"])
        in if null r then if null m then c else m else r

-- Current cube state, maximum depth, current depth, and a solution
findSolutionIterative :: Cube -> Int -> Int -> [String]
findSolutionIterative cube maxDepth depth
    | depth > maxDepth = []
    | otherwise = 
        let moves = findMoves cube depth []
        in if null moves then findSolutionIterative cube maxDepth (depth + 1) else moves
