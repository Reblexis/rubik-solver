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
        Nothing -> error ("Cubie" ++ show colorCubie ++ "not found")


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
cubieFromColorMove colorMove = getTransformation $ cubieFromColorCube $ colorMove CubeColors.solvedCube

{-
Move definitions.

Memoization optimizes this.
-}
data NamedMove = NamedMove {
    move :: Move,
    name :: String
} deriving Show

r :: NamedMove
r = NamedMove {move = cubieFromColorMove CubeColors.rMove, name = "R"}

l :: NamedMove
l = NamedMove {move = cubieFromColorMove CubeColors.lMove, name = "L"}

u :: NamedMove
u = NamedMove {move = cubieFromColorMove CubeColors.uMove, name = "U"}

d :: NamedMove
d = NamedMove {move = cubieFromColorMove CubeColors.dMove, name = "D"}

b :: NamedMove
b = NamedMove {move = cubieFromColorMove CubeColors.bMove, name = "B"}

f :: NamedMove
f = NamedMove {move = cubieFromColorMove CubeColors.fMove, name = "F"}

rPrime :: NamedMove
rPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.rMove), name = "R'"}

lPrime :: NamedMove
lPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.lMove), name = "L'"}

uPrime :: NamedMove
uPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.uMove), name = "U'"}

dPrime :: NamedMove
dPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.dMove), name = "D'"} 

bPrime :: NamedMove
bPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.bMove), name = "B'"}

fPrime :: NamedMove
fPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.fMove), name = "F'"}

r2 :: NamedMove
r2 = NamedMove {move = cubieFromColorMove (CubeColors.rMove . CubeColors.rMove), name = "R2"}

l2 :: NamedMove
l2 = NamedMove {move = cubieFromColorMove (CubeColors.lMove . CubeColors.lMove), name = "L2"}

u2 :: NamedMove
u2 = NamedMove {move = cubieFromColorMove (CubeColors.uMove . CubeColors.uMove), name = "U2"}

d2 :: NamedMove
d2 = NamedMove {move = cubieFromColorMove (CubeColors.dMove . CubeColors.dMove), name = "D2"}

b2 :: NamedMove
b2 = NamedMove {move = cubieFromColorMove (CubeColors.bMove . CubeColors.bMove), name = "B2"}

f2 :: NamedMove
f2 = NamedMove {move = cubieFromColorMove (CubeColors.fMove . CubeColors.fMove), name = "F2"}


possibleMoves :: [NamedMove]
possibleMoves = [r, l, u, d, b, f, rPrime, lPrime, uPrime, dPrime, bPrime, fPrime, r2, l2, u2, d2, b2, f2]


-- Cubies that are at the same position as their index and their rotation is 0
countCorrectCubies :: Cube -> Int
countCorrectCubies (Cube cubies) = length $ filter (\(cubie, index) -> position cubie == index && rotation cubie == 0) (zip cubies [0..19])
