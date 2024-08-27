{-
Cubie representation of rubik's cube. The positions are id'd this way:
- cube rotation: white on the front, blue on the top
- center cubies ignored
- from top left row-major order from front side to middle to back
- for each 'side' go from top left corner cubie clockwise (top left, top right,...., left mid)
-}
module CubeCubies where

import Data.List (elemIndex)
import Data.Array
import System.Random

import qualified CubeColors
import CubeColors (ColorCubie)

-- | Represents a single cubie with its position and rotation
data Cubie = Cubie {
    position :: Int,
    rotation :: Int -- 0 - 2 (could be two bits)
} deriving Show

-- | Represents the entire Rubik's Cube as a list of cubies
newtype Cube = Cube {
    cubies :: [Cubie]
} deriving Show

-- | Represents a solved Rubik's Cube
solvedCube :: Cube
solvedCube = Cube {
    cubies = map (`Cubie` 0) [0..19]
}

-- | Represents a move for a single cubie
data CubieMove = CubieMove {
    targetPosition :: Int,
    rotationAdd :: Int
} deriving Show

-- | Represents a move for the entire cube
newtype Move = Move {
    cubieMoves :: Array Int CubieMove
} deriving Show

-- | Default color configuration of cubies
defaultCubies :: [ColorCubie]
defaultCubies = CubeColors.getCubies CubeColors.solvedCube

-- | Calculate the rotation change for a cubie based on its color change
getRotationAdd :: ColorCubie -> ColorCubie -> Int
getRotationAdd (CubeColors.ColorCubie c1) (CubeColors.ColorCubie c2) =
    let
        firstColor = head c1
        firstColorIndex = elemIndex firstColor c2
    in case firstColorIndex of
        Just index -> index
        Nothing -> error "Color not found"

-- | Find a cubie in the list of all cubies based on its color configuration
findCubie :: ColorCubie -> [ColorCubie] -> Cubie
findCubie colorCubie allCubies =
    let
        cubieIndex = elemIndex colorCubie allCubies
    in case cubieIndex of
        Just index -> Cubie index (getRotationAdd colorCubie (allCubies !! index))
        Nothing -> error ("Cubie" ++ show colorCubie ++ "not found")


-- | Convert a color-based cube representation to a cubie-based representation
cubieFromColorCube :: CubeColors.Cube -> Cube
cubieFromColorCube colorCube =
    let
        colorCubies = CubeColors.getCubies colorCube
    in Cube $ map (`findCubie` colorCubies) defaultCubies


-- | Apply a move to a single cubie
applyCubieMove :: Cubie -> Move -> Cubie
applyCubieMove cubie move =
    let
        cubieMove = cubieMoves move ! position cubie
    in
        let
            newPosition = targetPosition cubieMove
            newRotation = (rotation cubie + rotationAdd cubieMove) `mod` (if newPosition < 8 then 3 else 2)
        in
            Cubie newPosition newRotation

-- | Apply a move to the entire cube
applyMove :: Cube -> Move -> Cube
applyMove cube move =
    let
        newCubies = map (`applyCubieMove` move) (cubies cube)
    in
        Cube newCubies

-- | Generate a move that transforms a solved cube into the given cube state
getTransformation :: Cube -> Move
getTransformation cube =
    let
        cubieMoves = map (\cubie -> CubieMove (position cubie) (rotation cubie)) (cubies cube)
    in
        Move $ listArray (0, 19) cubieMoves


-- | Convert a color-based move to a cubie-based move
cubieFromColorMove :: (CubeColors.Cube -> CubeColors.Cube) -> Move
cubieFromColorMove colorMove = getTransformation $ cubieFromColorCube $ colorMove CubeColors.solvedCube

-- | Represents a named move with its corresponding Move and string representation
data NamedMove = NamedMove {
    move :: Move,
    name :: String
} deriving Show

-- Move definitions (r, l, u, d, b, f, rPrime, lPrime, etc.)
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
rPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.rMove), name = "RP"}

lPrime :: NamedMove
lPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.lMove), name = "LP"}

uPrime :: NamedMove
uPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.uMove), name = "UP"}

dPrime :: NamedMove
dPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.dMove), name = "DP"}

bPrime :: NamedMove
bPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.bMove), name = "BP"}

fPrime :: NamedMove
fPrime = NamedMove {move = cubieFromColorMove (CubeColors.prime CubeColors.fMove), name = "FP"}

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


-- | List of all possible moves
possibleMoves :: [NamedMove]
possibleMoves = [r, l, u, d, b, f, rPrime, lPrime, uPrime, dPrime, bPrime, fPrime, r2, l2, u2, d2, b2, f2]

-- | List of possible moves in the G1 subgroup
possibleMovesG1 :: [NamedMove]
possibleMovesG1 = [r2, l2, u, uPrime, u2, d, dPrime, d2, f2, b2]

-- | Get a random move from a list of possible moves
getRandomMove :: [NamedMove] -> IO NamedMove
getRandomMove moves = do
    index <- randomRIO (0, length moves - 1)
    return $ moves !! index


-- | Count the number of cubies that are in their correct position and orientation
countCorrectCubies :: Cube -> Int
countCorrectCubies (Cube cubies) = length $ filter (\(cubie, index) -> position cubie == index && rotation cubie == 0) (zip cubies [0..19])


-- | List of indices representing the U-D slice of the cube
uDSlice :: [Int]
uDSlice = [9, 10, 17, 18]

-- | Check if the cube is in the G1 subset
-- This means that all cubies are correctly rotated (rotation 0) and the U-D slice edges are only in that slice.
isG1 :: Cube -> Bool
isG1 (Cube cubies) = all (\cubie -> rotation cubie == 0) cubies && all (\(cubie, index) -> notElem index uDSlice || elem (position cubie) uDSlice) (zip cubies [0..19])