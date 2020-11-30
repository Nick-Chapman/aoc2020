
module Day1 (main) where

import Misc

main :: IO ()
main = do
  xs <- readInts "input/day1.input"
  print ("day1, example1", check 654 $ calc1 1969)
  print ("day1, part1", check 3318632 $ part1 xs)
  print ("day1, example2", check 50346 $ calc2 100756)
  print ("day1, part2", check 4975084 $ part2 xs)

part1 :: [Int] -> Int
part1 = sum . map calc1

part2 :: [Int] -> Int
part2 = sum . map calc2

calc1 :: Int -> Int
calc1 x = x `div` 3 - 2

calc2 :: Int -> Int
calc2 x = do
  let f = calc1 x
  if f <= 0 then 0 else do
    f + calc2 f
