module Aoc.Day4 (part1, part2) where

import Aoc.Files (readWordSearch)
import Aoc.Matrix (down, left, line, pointValue, points, right, up)
import Aoc.Util (basicAnswer)
import Control.Monad ((>=>))
import Data.Maybe (mapMaybe)

part1 :: IO ()
part1 = basicAnswer readWordSearch calculate
  where
    calculate = sum . map countWords . points
    countWords p = length . filter (== "XMAS") . map (map pointValue . (\d -> line d 4 p)) $ directions
    directions = [up, down, left, right, up >=> left, up >=> right, down >=> left, down >=> right]

part2 :: IO ()
part2 = basicAnswer readWordSearch calculate
  where
    calculate = length . filter isXmas . points
    isXmas p = pointValue p == 'A' && cornersWork p
    cornersWork p = (diag1 == "MS" || diag1 == "SM") && (diag2 == "MS" || diag2 == "SM")
      where
        diag1 = mapMaybe (fmap pointValue) [up >=> left $ p, down >=> right $ p]
        diag2 = mapMaybe (fmap pointValue) [down >=> left $ p, up >=> right $ p]
