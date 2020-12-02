
module Day2 (main) where

import Data.Bits (xor)
import Misc (check)

main :: IO ()
main = do
  s <- readFile "input/day2.input"
  let res1 = length [ (line,v) | line <- lines s, let v = validate1 (cutup line), v]
  print ("day2, part1", check 564 res1)
  let res2 = length [ (line,v) | line <- lines s, let v = validate2 (cutup line), v]
  print ("day2, part2", check 325 res2)

validate1 ::(Int,Int,Char,String) -> Bool
validate1 (i,j,c,s) = do
  let n = length [ () | x <- s, x == c ]
  n>=i && n <=j

validate2 ::(Int,Int,Char,String) -> Bool
validate2 (i,j,c,s) = do
  let b1 = s !! (i-1) == c
  let b2 = s !! (j-1) == c
  b1 `xor` b2

cutup :: String -> (Int,Int,Char,String)
cutup s = do
  let s1 = words s
  let [a,bs,c] = s1
  let i :: Int = read $ takeWhile (not . (== '-')) a
  let j :: Int = read $ takeWhile (not . (== '-')) (tail( dropWhile (not . (== '-')) a))
  let b = head bs
  (i,j,b,c)
