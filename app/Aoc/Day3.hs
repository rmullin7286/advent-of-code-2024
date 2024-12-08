module Aoc.Day3 (part1, part2) where

import Aoc.Files (readMulInstructions)
import Aoc.Util (basicAnswerEither)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (foldl')
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, anySingle, errorBundlePretty, many, parse, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Instruction
  = Do
  | Dont
  | Mul !Int !Int

parseInstructions :: T.Text -> Either (ParseErrorBundle T.Text Data.Void.Void) [Instruction]
parseInstructions = parse parser "data/mul-instructions.txt"
  where
    parser = many loop
    loop = try instruction <|> try (anySingle >> loop)
    instruction = mul <|> do' <|> dont
    mul = Mul <$> (string "mul(" *> decimal) <*> (char ',' *> decimal <* char ')')
    do' = Do <$ string "do()"
    dont = Dont <$ string "don't()"

part1 :: IO ()
part1 = basicAnswerEither readMulInstructions calculate
  where
    f (Mul x y) = x * y
    f _ = 0
    calculate = bimap errorBundlePretty (sum . map f) . parseInstructions

part2 :: IO ()
part2 = basicAnswerEither readMulInstructions calculate
  where
    f (_, !total) Do = (True, total)
    f (_, !total) Dont = (False, total)
    f (False, !total) _ = (False, total)
    f (True, !total) (Mul x y) = (True, total + x * y)
    calculate = bimap errorBundlePretty (snd . foldl' f (True, 0)) . parseInstructions
