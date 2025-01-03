module Main where

import Aoc.Day1 qualified as Day1
import Aoc.Day2 qualified as Day2
import Aoc.Day3 qualified as Day3
import Aoc.Day4 qualified as Day4
import Aoc.Day5 qualified as Day5
import Aoc.Day6 qualified as Day6
import Aoc.Day7 qualified as Day7
import Control.Monad (join)
import Options.Applicative (CommandFields, Mod, Parser, command, execParser, idm, info, subparser)

answer :: String -> IO () -> Mod CommandFields (IO ())
answer name fn = command name (info (pure fn) idm)

commands :: Parser (IO ())
commands =
  subparser
    ( answer "day1-1" Day1.part1
        <> answer "day1-2" Day1.part2
        <> answer "day2-1" Day2.part1
        <> answer "day2-2" Day2.part2
        <> answer "day3-1" Day3.part1
        <> answer "day3-2" Day3.part2
        <> answer "day4-1" Day4.part1
        <> answer "day4-2" Day4.part2
        <> answer "day5-1" Day5.part1
        <> answer "day5-2" Day5.part2
        <> answer "day6-1" Day6.part1
        <> answer "day6-2" Day6.part2
        <> answer "day7-1" Day7.part1
        <> answer "day7-2" Day7.part2
    )

main :: IO ()
main = join $ execParser (info commands idm)
