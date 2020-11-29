
module Day1 (main) where

import Misc

main :: IO ()
main = do
  xs <- readInts "input/day1.input"
  print xs
  print ("day1, part1", check 123 $ part1 xs)

part1 :: [Int] -> Int
part1 = undefined
