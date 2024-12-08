module Aoc.Util (basicAnswer, basicAnswerEither) where

import TextShow (TextShow, printT)

basicAnswer :: (TextShow b) => IO a -> (a -> b) -> IO ()
basicAnswer reader compute = reader >>= printT . compute

basicAnswerEither :: (TextShow b) => IO a -> (a -> Either String b) -> IO ()
basicAnswerEither reader compute = reader >>= either putStrLn printT . compute
