
module Day1 where --(main) where

import Data.List (nub)
import Misc

main :: IO ()
main = do
  let sam1 = [1721, 979, 366, 299, 675, 1456]
  print ("day1, sam", check [514579] $ part1 sam1)
  xs <- readInts "input/day1.input"
  print ("day1, part1", check [788739] $ part1 xs)
  print ("day1, part2", check [178724430] $ part2 xs)

part1 :: [Int] -> [Int]
part1 xs =
  nub [ a * b |  a <- xs, b <- xs, a + b == 2020 ]

part2 :: [Int] -> [Int]
part2 xs =
  nub [ a * b * c |  a <- xs, b <- xs, c <- xs, a + b + c == 2020 ]
