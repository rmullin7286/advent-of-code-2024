module Aoc.Files (readLocationList, readFusionReport, readMulInstructions) where

import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, many, runParser, sepBy1, sepEndBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

readLocationList :: IO [(Int, Int)]
readLocationList = TIO.readFile locationList <&> (either (error . errorBundlePretty) id . parseLocationList)

locationList :: FilePath
locationList = "data/locations.txt"

parseLocationList :: T.Text -> Either (ParseErrorBundle T.Text Void) [(Int, Int)]
parseLocationList = runParser input locationList
  where
    input = line `sepEndBy` newline
    line = (,) <$> decimal <* ws <*> decimal
    ws = void $ many $ char ' '

fusionReport :: FilePath
fusionReport = "data/fusion-report.txt"

parseFusionReport :: T.Text -> Either (ParseErrorBundle T.Text Void) [[Int]]
parseFusionReport = runParser input fusionReport
  where
    input = line `sepEndBy` newline
    line = decimal `sepBy1` char ' '

readFusionReport :: IO [[Int]]
readFusionReport = TIO.readFile fusionReport <&> (either (error . errorBundlePretty) id . parseFusionReport)

-- Since the point of this problem is the parsing, I'm leaving that to the actual problem itself.
readMulInstructions :: IO T.Text
readMulInstructions = TIO.readFile "data/mul-instructions.txt"
