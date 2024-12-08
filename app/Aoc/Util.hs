module Aoc.Util (basicAnswer) where

import TextShow (TextShow, printT)

basicAnswer :: (TextShow b) => IO a -> (a -> b) -> IO ()
basicAnswer reader compute = reader >>= printT . compute
