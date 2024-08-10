{-
Cubie represantation of rubik's cube. The positions are id'd this way:
- cube rotation: white on the front, blue on the top
- center cubies ignored
- from top left row-major order from front side to middle to back
- for each 'side' go from top left corner cubie clockwise (top left, top right,...., left mid)
-}
module CubeCubies where

import qualified Data.Vector as V

import qualified CubeColors as CubeColors
import CubeColors (Color)

data Cubie = Cubie {
    position :: Int,
    rotation :: Int -- 0 - 2 (could be two bits)
} deriving Show

newtype Cube = Cube {
    cubies :: V.Vector Cubie
} deriving Show

solvedCube :: Cube
solvedCube = Cube {
    cubies = V.fromList $ map (\x -> Cubie x 0) [0..19]
}

{-
Where should the given cubie go and what should be added to rotation (mod 3 or 2).
-}

data CubieMove = CubieMove {
    targetPosition :: Int,
    rotationAdd :: Int
}

newtype Move = Move {
    cubieMoves :: V.Vector CubieMove
}




--cubieFromColor :: CubeColors.Cube -> Cube
--cubieFromColor colorCube = 
