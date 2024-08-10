{-
Cubie represantation of rubik's cube. The positions are id'd this way:
- cube rotation: white on the front, blue on the top
- center cubies ignored
- from top left row-major order from front side to middle to back
- for each 'side' go from top left corner cubie clockwise (top left, top right,...., left mid)
-}
module CubeCubies where

import Data.List (elemIndex)
import Data.Array

import qualified CubeColors
import CubeColors (Color(..), ColorCubie)

data Cubie = Cubie {
    position :: Int,
    rotation :: Int -- 0 - 2 (could be two bits)
} deriving Show

newtype Cube = Cube {
    cubies :: [Cubie]
} deriving Show

solvedCube :: Cube
solvedCube = Cube {
    cubies = map (`Cubie` 0) [0..19]
}

{-
Where should the given cubie go and what should be added to rotation (mod 3 or 2).
-}

data CubieMove = CubieMove {
    targetPosition :: Int,
    rotationAdd :: Int
} deriving Show

newtype Move = Move {
    cubieMoves :: Array Int CubieMove
} deriving Show

defaultCubies :: [ColorCubie]
defaultCubies = CubeColors.getCubies CubeColors.solvedCube

-- Check the position change of the first element in the first list
getRotationAdd :: ColorCubie -> ColorCubie -> Int
getRotationAdd (CubeColors.ColorCubie c1) (CubeColors.ColorCubie c2) =
    let
        firstColor = head c1
        firstColorIndex = elemIndex firstColor c2
    in case firstColorIndex of
        Just index -> index
        Nothing -> error "Color not found"

findCubie :: ColorCubie -> [ColorCubie] -> Cubie
findCubie colorCubie allCubies =
    let
        cubieIndex = elemIndex colorCubie allCubies
    in case cubieIndex of
        Just index -> Cubie index (getRotationAdd colorCubie (allCubies !! index))
        Nothing -> error "Cubie not found"


cubieFromColorCube :: CubeColors.Cube -> Cube
cubieFromColorCube colorCube =
    let
        colorCubies = CubeColors.getCubies colorCube
    in Cube $ map (`findCubie` colorCubies) defaultCubies


applyCubieMove :: Cubie -> Move -> Cubie
applyCubieMove cubie move =
    let 
        cubieMove = cubieMoves move ! position cubie
    in
        let
            newPosition = targetPosition cubieMove
            newRotation = (rotation cubie + rotationAdd cubieMove) `mod` 3
        in 
            Cubie newPosition newRotation

-- Applies different cubie move to each cubie depending on its position
applyMove :: Cube -> Move -> Cube
applyMove cube move =
    let
        newCubies = map (`applyCubieMove` move) (cubies cube)
    in
        Cube newCubies

{-
Returns move that transforms solved cube into the given cube.
-}
getTransformation :: Cube -> Move
getTransformation cube =
    let
        cubieMoves = map (\cubie -> CubieMove (position cubie) (rotation cubie)) (cubies cube)
    in
        Move $ listArray (0, 19) cubieMoves


cubieFromColorMove :: (CubeColors.Cube -> CubeColors.Cube) -> Move
cubieFromColorMove move = getTransformation $ cubieFromColorCube $ move CubeColors.solvedCube

{-
Move definitions.

Memoization optimizes this.
-}
r :: Move
r = cubieFromColorMove CubeColors.rMove
l :: Move
l = cubieFromColorMove CubeColors.lMove
u :: Move
u = cubieFromColorMove CubeColors.uMove
d :: Move
d = cubieFromColorMove CubeColors.dMove
b :: Move
b = cubieFromColorMove CubeColors.bMove
f :: Move
f = cubieFromColorMove CubeColors.fMove
rp :: Move
rp = cubieFromColorMove (CubeColors.prime CubeColors.rMove)
lp :: Move
lp = cubieFromColorMove (CubeColors.prime CubeColors.lMove)
up :: Move
up = cubieFromColorMove (CubeColors.prime CubeColors.uMove)
dp :: Move
dp = cubieFromColorMove (CubeColors.prime CubeColors.dMove)
bp :: Move
bp = cubieFromColorMove (CubeColors.prime CubeColors.bMove)
fp :: Move
fp = cubieFromColorMove (CubeColors.prime CubeColors.fMove)
r2 :: Move
r2 = cubieFromColorMove (CubeColors.rMove . CubeColors.rMove)
l2 :: Move
l2 = cubieFromColorMove (CubeColors.lMove . CubeColors.lMove)
u2 :: Move
u2 = cubieFromColorMove (CubeColors.uMove . CubeColors.uMove)
d2 :: Move
d2 = cubieFromColorMove (CubeColors.dMove . CubeColors.dMove)
b2 :: Move
b2 = cubieFromColorMove (CubeColors.bMove . CubeColors.bMove)
f2 :: Move
f2 = cubieFromColorMove (CubeColors.fMove . CubeColors.fMove)
