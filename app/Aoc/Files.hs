module Aoc.Files
  ( readLocationList,
    readFusionReport,
    readMulInstructions,
    readWordSearch,
    readPrintQueue,
    PrintQueue (..),
    readBridgeEquations,
    readGuardDuty,
  )
where

import Aoc.Matrix (Matrix2, mkMatrix)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Void (Void)
import System.IO (readFile')
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, many, runParser, sepBy1, sepEndBy, sepEndBy1)
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

readWordSearch :: IO (Matrix2 Char)
readWordSearch = mkMatrix . V.fromList . map V.fromList . lines <$> readFile "data/xmas-word-search.txt"

data PrintQueue = PrintQueue [(Int, Int)] [[Int]]
  deriving (Show)

printQueue :: FilePath
printQueue = "data/print-queue.txt"

parsePrintQueue :: T.Text -> Either (ParseErrorBundle T.Text Void) PrintQueue
parsePrintQueue = runParser parser printQueue
  where
    parser = PrintQueue <$> (rules <* newline) <*> orders
    rules = rule `sepEndBy1` newline
    rule = (,) <$> (decimal <* char '|') <*> decimal
    orders = order `sepEndBy1` newline
    order = decimal `sepBy1` char ','

readPrintQueue :: IO PrintQueue
readPrintQueue = TIO.readFile printQueue <&> (either (error . errorBundlePretty) id . parsePrintQueue)

bridgeEquations :: FilePath
bridgeEquations = "data/bridge-equations.txt"

parseBridgeEquations :: T.Text -> Either (ParseErrorBundle T.Text Void) [(Int, [Int])]
parseBridgeEquations = runParser parser bridgeEquations
  where
    parser = equation `sepEndBy1` newline
    equation = (,) <$> (decimal <* char ':') <*> many (char ' ' *> decimal)

readBridgeEquations :: IO [(Int, [Int])]
readBridgeEquations = TIO.readFile bridgeEquations <&> (either (error . errorBundlePretty) id . parseBridgeEquations)

readGuardDuty :: IO String
readGuardDuty = readFile' "data/guard-duty.txt"
