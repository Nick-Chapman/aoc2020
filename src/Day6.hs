
module Day6 (main) where

import Data.List (nub)
import Data.List.Split (splitWhen)
import Misc (check)

main :: IO ()
main = do
  sam <- readFile "input/day6.input.sam"
  s <- readFile "input/day6.input"
  let !_ = check 11 $ part1 sam
  let !_ = check 6 $ part2 sam
  print ("day6, part1", check 6457 $ part1 s)
  print ("day6, part2", check 3260 $ part2 s)
  where
    parse = splitWhen (=="") . lines
    part1 = sum . map (length . pt1) . parse
    part2 = sum . map (length . pt2) . parse
    pt1 = nub . concat
    pt2 xs = do
      let n = length xs
      let ys = concat xs
      [ c | c <- ['a'..'z'], n == length (filter (==c) ys) ]
