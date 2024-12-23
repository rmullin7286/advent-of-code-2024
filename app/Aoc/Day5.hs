module Aoc.Day5 (part1, part2) where

import Aoc.Files (PrintQueue (PrintQueue), readPrintQueue)
import Aoc.Util (basicAnswer)
import Control.Lens (Bifunctor (bimap))
import Data.Function (on)
import Data.IntMap qualified as M
import Data.List (foldl', groupBy, sortBy)
import Data.Set qualified as S
import Data.Tuple (swap)

part1 :: IO ()
part1 = basicAnswer readPrintQueue calculate
  where
    calculate (PrintQueue rules orders) = sum . map middle . filter (orderWorks $ getRuleMap rules) $ orders

part2 :: IO ()
part2 = basicAnswer readPrintQueue calculate
  where
    calculate (PrintQueue rules orders) =
      sum
        . map (middle . sortBy ruleOrder)
        . filter (not . orderWorks ruleMap)
        $ orders
      where
        ruleMap = getRuleMap rules
        ruleOrder x y
          | x `S.member` getRule y ruleMap = LT
          | y `S.member` getRule x ruleMap = GT
          | otherwise = EQ

type RuleMap = M.IntMap (S.Set Int)

getRuleMap :: [(Int, Int)] -> RuleMap
getRuleMap =
  M.fromList
    . map (bimap head S.fromList . unzip)
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . map swap

getRule :: Int -> RuleMap -> S.Set Int
getRule = M.findWithDefault S.empty

orderWorks :: RuleMap -> [Int] -> Bool
orderWorks rules = snd . foldl' go (S.empty, True)
  where
    go (forbidden, ok) x
      | not ok = (forbidden, False)
      | x `S.member` forbidden = (forbidden, False)
      | otherwise = (forbidden <> getRule x rules, True)

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)
