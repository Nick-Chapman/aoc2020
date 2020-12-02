
module Day2 (main) where

import Data.Bits (xor)
import Misc (check)
import Par (Par,parse,many0,int,key,char,ident,space,nl)

main :: IO ()
main = do
  s <- readFile "input/day2.input"
  let xs = parse gram s
  let res1 = length (filter validate1 xs)
  print ("day2, part1", check 564 res1)
  let res2 = length (filter validate2 xs)
  print ("day2, part2", check 325 res2)

gram :: Par [(Int,Int,Char,String)]
gram = many0 $ do
  i <- int; key "-"
  j <- int; space
  c <- char; key ":"; space
  s <- ident; nl
  pure $ (i,j,c,s)

validate1 ::(Int,Int,Char,String) -> Bool
validate1 (i,j,c,s) = do
  let n = length [ () | x <- s, x == c ]
  n>=i && n <=j

validate2 ::(Int,Int,Char,String) -> Bool
validate2 (i,j,c,s) = do
  let b1 = s !! (i-1) == c
  let b2 = s !! (j-1) == c
  b1 `xor` b2
