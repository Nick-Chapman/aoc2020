module Advent2020 where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8

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
  , Day6.main
  , Day7.main
  , Day8.main
  ]
