
module Day25 (main) where

import Misc (check)
import Par (Par,parse,nl,int)

main :: IO ()
main = do
  let sam = (5764801,17807724)
  inp <- parse gram <$> readFile "input/day25.input"
  print $ check 14897079 $ part1 sam
  print $ check 15467093 $ part1 inp

gram :: Par (Int,Int)
gram = do x <- int; nl; y <- int; pure (x,y)

part1 :: (Int,Int) -> Int
part1 (x,y) = do
  let c = crack x 1 0
  let ekc = pow y 1 c
  ekc

crack :: Int -> Int -> Int -> Int
crack x acc n = do
  if acc == x then n else do
    let acc' = 7 * acc `mod` m
    crack x acc' (n+1)

pow :: Int -> Int -> Int -> Int
pow x acc n = do
  if n == 0 then acc else do
    let acc' = x * acc `mod` m
    pow x acc' (n-1)

m :: Int
m = 20201227
