module Aoc.Day2 (part1, part2) where

import Aoc.Files (readFusionReport)
import Aoc.Util (basicAnswer)
import Data.Bifunctor (Bifunctor (bimap))

part1 :: IO ()
part1 = basicAnswer readFusionReport calculate
  where
    calculate = length . filter isSafe

part2 :: IO ()
part2 = basicAnswer readFusionReport calculate
  where
    calculate = length . filter (any isSafe . removeOneAtATime)

isSafe :: [Int] -> Bool
isSafe =
  uncurry (&&)
    . bimap allEqual (all $ \x -> x > 0 && x < 4)
    . unzip
    . map (\(x, y) -> (compare x y, difference x y))
    . pairs

pairs :: [a] -> [(a, a)]
pairs (x : y : xs) = (x, y) : pairs (y : xs)
pairs _ = []

difference :: Int -> Int -> Int
difference x y = abs $ x - y

allEqual :: (Eq a) => [a] -> Bool
allEqual (x : xs) = all (== x) xs
allEqual _ = False

removeAt :: Int -> [a] -> [a]
removeAt 0 (_ : xs) = xs
removeAt i (x : xs) = x : removeAt (i - 1) xs
removeAt _ [] = []

removeOneAtATime :: [a] -> [[a]]
removeOneAtATime xs = map (`removeAt` xs) [0 .. length xs]
