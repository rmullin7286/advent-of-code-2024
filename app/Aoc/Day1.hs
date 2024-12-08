module Aoc.Day1 (part1, part2) where

import Aoc.Files (readLocationList)
import Aoc.Util (basicAnswer)
import Data.Bifunctor (Bifunctor (bimap))
import Data.IntMap.Strict qualified as IM
import Data.List (foldl', sort)

part1 :: IO ()
part1 = basicAnswer readLocationList calculate
  where
    calculate =
      sum
        . map abs
        . uncurry (zipWith (-))
        . bimap sort sort
        . unzip

part2 :: IO ()
part2 = basicAnswer readLocationList calculate
  where
    calculate input = foldl' accum 0 left
      where
        (left, right) = unzip input
        frequency = foldl' (\freq i -> IM.insertWith (+) i 1 freq) IM.empty right
        accum total x = total + (x * IM.findWithDefault 0 x frequency)
