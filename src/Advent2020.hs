module Advent2020 where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2

main :: IO ()
main = do
  getArgs >>= \case
    [] -> mapM_ id mains
    args -> mapM_ (\day -> mains !! (read day - 1)) args

mains :: [IO ()]
mains =
  [ Day1.main
  , Day2.main
  ]
