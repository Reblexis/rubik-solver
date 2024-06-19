{-
Rubik's cube is represented in the following way:
A tuple of tuples with fixed sizes, so data = (BACK, TOP, LEFT, FRONT, RIGHT, BOTTOM)
and BACK,TOP,..,BOTTOM = (UL, UM, UR, ML, MM, MR, BL, BM, BB) if we rotate to it from the front
    side via the shortest path (and to the back side we rotate through top side).
    And colors are represented as integers via the following mapping: Y,B,O,W,R,G = 0,1,2,3,4,5
-}
-- Define the Side and Cube types
data Side = Side (Int, Int, Int, Int, Int, Int, Int, Int, Int) deriving Show
data Cube = Cube (Side, Side, Side, Side, Side, Side) deriving Show

-- Function to rotate a Side clockwise
rotateSideC :: Side -> Side
rotateSideC (Side (ul, um, ur, ml, mm, mr, bl, bm, br)) =
    Side (bl, ml, ul, bm, mm, um, br, mr, ur)

-- Function to rotate a Side counterclockwise
rotateSideCC :: Side -> Side
rotateSideCC side = rotateSideC $ rotateSideC $ rotateSideC side

-- Function to modify the right column from another side's right column
changeRightFrom :: Side -> Side -> Side
changeRightFrom (Side (ul1, um1, _, ml1, mm1, _, bl1, bm1, _)) (Side (_, _, ur2, _, _, mr2, _, _, br2)) =
    Side (ul1, um1, ur2, ml1, mm1, mr2, bl1, bm1, br2)

-- Function to perform an R move on the cube (rotates the right side and adjusts adjacent faces)
rMove :: Cube -> Cube
rMove (Cube (back, top, left, front, right, bottom)) =
    Cube (changeRightFrom back top, changeRightFrom top front, left, changeRightFrom front bottom, rotateSideC right, changeRightFrom bottom back)

-- Modify the middle column from another side's middle column
changeMiddleFrom :: Side -> Side -> Side
changeMiddleFrom (Side (ul1, _, ur1, ml1, _, mr1, bl1, _, br1)) (Side (_, um2, _, _, mm2, _, _, bm2, _)) =
    Side (ul1, um2, ur1, ml1, mm2, mr1, bl1, bm2, br1)

-- Function to perform an M move on the cube (rotates the middle slice)
mMove :: Cube -> Cube
mMove (Cube (back, top, left, front, right, bottom)) =
    Cube (changeMiddleFrom back top, changeMiddleFrom top front, left, changeMiddleFrom front bottom, right, changeMiddleFrom bottom back)

-- Function to rotate the entire cube clockwise
rotateClockwise :: Cube -> Cube
rotateClockwise (Cube (back, top, left, front, right, bottom)) =
    Cube (rotateSideCC back, rotateSideC left, rotateSideC bottom, rotateSideC front, rotateSideC top, rotateSideCC right)

-- Function to check if a side is one color
isOneColor :: Side -> Bool
isOneColor (Side (a, b, c, d, e, f, g, h, i)) = all (== a) [b, c, d, e, f, g, h, i]

-- Function to check if the entire cube is solved
isSolved :: Cube -> Bool
isSolved (Cube (back, top, left, front, right, bottom)) =
    all isOneColor [back, top, left, front, right, bottom]

-- A representation of a solved cube
solvedCube :: Cube
solvedCube = Cube (
    Side (0,0,0,0,0,0,0,0,0),
    Side (1,1,1,1,1,1,1,1,1),
    Side (2,2,2,2,2,2,2,2,2),
    Side (3,3,3,3,3,3,3,3,3),
    Side (4,4,4,4,4,4,4,4,4),
    Side (5,5,5,5,5,5,5,5,5)
    )
