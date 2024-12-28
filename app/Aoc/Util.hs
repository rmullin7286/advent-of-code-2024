module Aoc.Util (basicAnswer, basicAnswerEither, unfoldl) where

import TextShow (TextShow, printT)

basicAnswer :: (TextShow b) => IO a -> (a -> b) -> IO ()
basicAnswer reader compute = reader >>= printT . compute

basicAnswerEither :: (TextShow b) => IO a -> (a -> Either String b) -> IO ()
basicAnswerEither reader compute = reader >>= either putStrLn printT . compute

unfoldl :: (b -> Maybe (a, b)) -> b -> [a]
unfoldl f x = case f x of
  Just (y, z) -> y : unfoldl f z
  Nothing -> []
