{-
Rubik's cube is represented in the following way:
A tuple of tuples with fixed sizes, so data = (BACK, TOP, LEFT, FRONT, RIGHT, BOTTOM)
and BACK,TOP,..,BOTTOM = (UL, UM, UR, ML, MM, MR, BL, BM, BB) if we rotate to it from the front
    side via the shortest path (and to the back side we rotate through top side).
    And colors are represented as integers via the following mapping: Y,B,O,W,R,G = 0,1,2,3,4,5
-}
rotateSideC (ul, um, ur, ml, mm, mr, bl, bm, br) = (bl, ml, ul, bm, mm, um, br, mr, ur)
rotateSideCC side = rotateSideC $ rotateSideC $ rotateSideC side

changeRightFrom (ul1, um1, ur1, ml1,mm1, mr1, bl1, bm1, br1) (ul2, um2, ur2, ml2, mm2, mr2, bl2, bm2, br2)=
    (ul1, um1, ur2, ml1, mm1, mr2, bl1, bm1, br2)

rMove (back, top, left, front, right, bottom) = (changeRightFrom back top, changeRightFrom top front, left, changeRightFrom front bottom, rotateSideC right, changeRightFrom bottom back)

changeMiddleFrom (ul1, um1, ur1, ml1, mm1, mr1, bl1, bm1, br1) (ul2, um2, ur2, ml2, mm2, mr2, bl2, bm2, br2)=
    (ul1, um2, ur1, ml1, mm2, mr1, bl1, bm2, br1)

mMove (back, top, left, front, right, bottom) = (changeMiddleFrom back top, changeMiddleFrom top front, left, changeMiddleFrom front bottom, right, changeMiddleFrom bottom left)

rotateClockwise (back, top, left, front, right, bottom) = (rotateSideCC back, rotateSideC left, rotateSideC bottom, rotateSideC front, rotateSideC top, rotateSideCC right)

isOneColor side = all (==head side) side 
isSolved cube = all isOneColor cube
