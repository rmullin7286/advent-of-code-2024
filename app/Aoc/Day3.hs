module Aoc.Day3 (run) where

import Aoc.Files (readMulInstructions)
import Aoc.Util (basicAnswer)
import Control.Monad (void)
import Data.Either (fromRight)
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle (ParseErrorBundle), anySingle, anySingleBut, errorBundlePretty, many, parse, parseTest, skipMany, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

data MulInstruction = MulInstruction !Int !Int
  deriving (Show)

part1 :: IO ()
part1 = basicAnswer readMulInstructions calculate
  where
    calculate :: T.Text -> Int
    calculate = undefined

parseMulInstructions :: T.Text -> [MulInstruction]
parseMulInstructions = fromRight [] . runParser

runParser :: T.Text -> Either (ParseErrorBundle T.Text Void) [MulInstruction]
runParser = parse parser ""
  where
    parser = many loop
    mul = MulInstruction <$> (string "mul(" *> decimal) <*> (char ',' *> decimal <* char ')')
