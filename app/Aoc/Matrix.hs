module Aoc.Matrix
  ( Matrix2,
    Point,
    mkMatrix,
    pointValue,
    up,
    down,
    left,
    right,
    points,
    line,
    movePoint,
    matrixFromLists,
    pointAt,
    coordinates,
  )
where

import Data.Vector (Vector)
import Data.Vector qualified as V

data Matrix2 a = Matrix2 !Int !(Vector (Vector a))

mkMatrix :: Vector (Vector a) -> Matrix2 a
mkMatrix v = Matrix2 width v
  where
    width = V.minimum . V.map V.length $ v

matrixFromLists :: [[a]] -> Matrix2 a
matrixFromLists = mkMatrix . V.fromList . map V.fromList

height :: Matrix2 a -> Int
height (Matrix2 _ v) = V.length v

data Point a = Point (Matrix2 a) Int Int

pointAt :: Int -> Int -> Matrix2 a -> Maybe (Point a)
pointAt i j m@(Matrix2 width _) =
  if i >= 0 && i < height m && j >= 0 && j < width
    then Just $ Point m i j
    else Nothing

pointValue :: Point a -> a
pointValue (Point (Matrix2 _ v) i j) = v V.! i V.! j

coordinates :: Point a -> (Int, Int)
coordinates (Point _ i j) = (i, j)

movePoint :: Int -> Int -> Point a -> Maybe (Point a)
movePoint i' j' (Point m@(Matrix2 width _) i j) =
  if newi >= 0 && newi < height m && newj >= 0 && newj < width
    then Just $ Point m newi newj
    else Nothing
  where
    newi = i + i'
    newj = j + j'

type Direction a = Point a -> Maybe (Point a)

up :: Direction a
up = movePoint (-1) 0

down :: Direction a
down = movePoint 1 0

left :: Direction a
left = movePoint 0 (-1)

right :: Direction a
right = movePoint 0 1

points :: Matrix2 a -> [Point a]
points m@(Matrix2 width _) = [Point m i j | i <- [0 .. height m - 1], j <- [0 .. width - 1]]

line :: Direction a -> Int -> Point a -> [Point a]
line _ 0 _ = []
line d x p = case d p of
  Just p' -> p : line d (x - 1) p'
  Nothing -> [p]
