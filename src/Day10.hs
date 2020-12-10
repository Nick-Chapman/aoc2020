
module Day10 (main) where

import Data.List (sort)
import Data.List.Split (splitWhen)
import Misc (readInts,check)

main :: IO ()
main = do
  sama <- readInts "input/day10.input.sama"
  samb <- readInts "input/day10.input.samb"
  inp <- readInts "input/day10.input"
  let !_ = check 35 $ part1 sama
  let !_ = check 220 $ part1 samb
  print ("day10, part1", check 2240 $ part1 inp)
  let !_ = check 8 $ part2 sama
  let !_ = check 19208 $ part2 samb
  print ("day10, part2", check 99214346656768 $ part2 inp)

part1 :: [Int] -> Int
part1 xs = do
  let ys = sort xs
  let ds = [ b-a | (a,b) <- zip (0:ys) (ys++[last ys+3]) ]
  let a = length [ d | d <- ds, d == 1 ]
  let b = length [ d | d <- ds, d == 3 ]
  a * b

part2 :: [Int] ->Int
part2 xs = do
  let ys = sort xs
  let ds = [ b-a | (a,b) <- zip (0:ys) (ys++[last ys+3]) ]
  product [ fib3 (length run) | run <- splitWhen (==3) ds ]
  where
    fib3 :: Int -> Int -- sequence defined by summing previous 3 numbers
    fib3 n = if
      | n < 0 -> 0
      | n == 0 -> 1
      | otherwise -> fib3 (n-1) + fib3 (n-2) + fib3 (n-3)
