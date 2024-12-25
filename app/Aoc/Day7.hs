module Aoc.Day7 (part1, part2) where

import Aoc.Files (readBridgeEquations)
import Aoc.Util (basicAnswer)

part1 :: IO ()
part1 = basicAnswer readBridgeEquations (sharedCalculate [(*), (+)])

part2 :: IO ()
part2 = basicAnswer readBridgeEquations (sharedCalculate [(*), (+), (|||)])

type Op = Int -> Int -> Int

sharedCalculate :: [Op] -> [(Int, [Int])] -> Int
sharedCalculate ops = sum . map fst . filter isPossible
  where
    isPossible (_, []) = False
    isPossible (target, x : xs) = isPossible' x xs
      where
        isPossible' cur [] = cur == target
        isPossible' cur (x' : xs') =
          cur <= target
            && ( any (`isPossible'` xs')
                   . filter (target >=)
                   . map (\op -> cur `op` x')
                   $ ops
               )

-- equivalent to the || operator in the question, but that operator is already taken in Haskell.
(|||) :: Int -> Int -> Int
x ||| y = (x * (10 ^ tensPlace y)) + y

tensPlace :: Int -> Int
tensPlace 0 = 0
tensPlace x = 1 + tensPlace (x `div` 10)
