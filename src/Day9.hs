
module Day9 (main) where

import Misc (check)
import Par (Par,parse,nl,separated,int)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day9.input.sam"
  inp <- parse gram <$> readFile "input/day9.input"
  let !_ = check [127] $ part1 5 sam
  print ("day9, part1", check [29221323] $ part1 25 inp)
  let !_ = check 62 $ part2 127 sam
  print ("day9, part2", check 4389369 $ part2 29221323 inp)
  pure ()

part1 :: Int -> [Int] -> [Int]
part1 n xs =
  [ q
  | ys <- tails xs
  , length ys >= n+1
  , let pre = take n ys
  , let q = head (drop n ys)
  , let sums = [ a + b | a <- pre, b <- pre, a /= b ]
  , let b = q `elem` sums
  , not b
  ]

part2 :: Int -> [Int] -> Int
part2 target xs = loop [] 0 xs xs
  where
    loop :: [Int] -> Int -> [Int] -> [Int] -> Int
    loop sub sum ys zs = if
      | sum > target -> loop (tail sub) (sum - head sub) (tail ys) zs
      | sum < target -> loop (sub ++ [head zs]) (sum + head zs) ys (tail zs)
      | otherwise -> minimum sub + maximum sub

tails :: [a] -> [[a]]
tails xs = xs : case xs of [] -> []; _:xs -> tails xs

gram :: Par [Int]
gram = separated nl int
