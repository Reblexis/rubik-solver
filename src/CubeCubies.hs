{-
Cubie represantation of rubik's cube. The positions are id'd this way:
- cube rotation: white on the front, blue on the top
- center cubies ignored
- from top left row-major order from front side to middle to back
- for each 'side' go from top left corner cubie clockwise (top left, top right,...., left mid)
-}
module CubeCubies where

import Data.List (elemIndex)

import qualified CubeColors as CubeColors
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
}

newtype Move = Move {
    cubieMoves :: [CubieMove]
}

defaultCubies :: [ColorCubie]
defaultCubies =
    let colors = [[White,Blue,Orange],[White,Blue],[White,Blue,Red],[White,Orange],[White,Red],
                [White,Green,Orange],[White,Green],[White,Green,Red],[Blue,Orange],[Blue,Red],
                [Green,Orange],[Green,Red],[Yellow,Blue,Orange],[Yellow,Blue],[Yellow,Blue,Red],
                [Yellow,Orange],[Yellow,Red],[Yellow,Green,Orange],[Yellow,Green],[Yellow,Green,Red]]
    in map CubeColors.ColorCubie colors

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


cubieFromColor :: CubeColors.Cube -> Cube
cubieFromColor colorCube =
    let
        colorCubies = CubeColors.getCubies colorCube
    in Cube $ map (`findCubie` defaultCubies) colorCubies
