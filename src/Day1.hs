
module Day1 where --(main) where

import Misc

main :: IO ()
main = do
  xs <- readInts "input/day1.input"
  print xs
  print $ length xs
  print ("day1, example1", check 123 $ calc1 1000)
  --print ("day1, part1", check 99 $ part1 xs)

part1 :: [Int] -> Int
part1 = undefined calc1

calc1 :: Int -> Int
calc1 = undefined
