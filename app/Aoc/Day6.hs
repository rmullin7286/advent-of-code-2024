module Aoc.Day6 (part1, part2) where

import Aoc.Files (readGuardDuty)
import Aoc.Matrix (Matrix2, Point, coordinates, down, left, matrixFromLists, pointValue, points, right, up)
import Aoc.Util (basicAnswer, unfoldl)
import Control.Monad (mfilter)
import Data.Foldable (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as S
import Prelude hiding (Left, Right)

data MapElem
  = Empty
  | Obstacle
  | Start
  deriving (Eq, Ord)

data GuardDirection
  = Up
  | Right
  | Down
  | Left
  deriving (Eq, Ord, Show)

rotate :: GuardDirection -> GuardDirection
rotate Up = Right
rotate Right = Down
rotate Down = Left
rotate Left = Up

move :: GuardDirection -> Point a -> Maybe (Point a)
move Up = up
move Right = right
move Left = left
move Down = down

part1 :: IO ()
part1 = basicAnswer readGuardDuty calculate
  where
    calculate = countPath . guardMatrix
    countPath :: Matrix2 MapElem -> Int
    countPath m = countPath' Up 1 start $ S.singleton $ coordinates start
      where
        start = fromMaybe (error "No start value") . find ((== Start) . pointValue) . points $ m
        countPath' dir count cur visited = case pointValue <$> next of
          Nothing -> count + 1
          Just Obstacle -> countPath' (rotate dir) count cur visited
          _ ->
            if coordinates cur `S.member` visited
              then countPath' dir count (fromJust next) visited
              else countPath' dir (count + 1) (fromJust next) (S.insert (coordinates cur) visited)
          where
            next = move dir cur

part2 :: IO ()
part2 = basicAnswer readGuardDuty calculate
  where
    calculate input = length . filter isLoop . filter ((== Empty) . pointValue) . points $ matrix
      where
        matrix = guardMatrix input
        start = fromMaybe (error "No start position found") . find ((== Start) . pointValue) . points $ matrix
        isLoop p = isLoop' S.empty Up start
          where
            isLoop' visited dir cur
              -- We've already been here
              | (dir, coordinates cur) `S.member` visited = True
              -- We've left the map
              | Nothing <- next = False
              -- We've hit an obstacle
              | Just n <- next, Obstacle <- pointValue n = isLoop' (S.insert (dir, coordinates cur) visited) (rotate dir) cur
              -- We've hit the bonus wall
              | Just n <- next, coordinates n == coordinates p = isLoop' (S.insert (dir, coordinates cur) visited) (rotate dir) cur
              -- Continue
              | Just n <- next = isLoop' (S.insert (dir, coordinates cur) visited) dir n
              where
                next = move dir cur

guardMatrix :: String -> Matrix2 MapElem
guardMatrix = matrixFromLists . map (map toElem) . lines
  where
    toElem '^' = Start
    toElem '#' = Obstacle
    toElem '.' = Empty
    toElem c = error $ "Unknown element: " ++ show c
