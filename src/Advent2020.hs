module Advent2020 where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

main :: IO ()
main = do
  getArgs >>= \case
    [] -> mapM_ id mains
    args -> mapM_ (\day -> mains !! (read day - 1)) args

mains :: [IO ()]
mains =
  [ Day1.main
  , Day2.main
  , Day3.main
  , Day4.main
  , Day5.main
  ]
