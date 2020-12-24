
module Day24 (main) where

import Control.Applicative (some)
import Misc (check)
import Par (Par,parse,separated,nl,alts,lit)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day24.input.sam"
  inp <- parse gram <$> readFile "input/day24.input"
  print $ check 10 $ part1 sam
  print ("day24, part1", check 420 $ part1 inp)

part1 :: [[Dir]] -> Int
part1 input = do
  let ps = [ foldl step (0,0) xs | xs <- input ]
  let m = Map.fromListWith (+) [ (p,1::Int) | p <- ps ]
  let cs = Map.elems m
  let odds = [ c | c <- cs , c `mod` 2 == 1 ]
  length odds

step :: Pos -> Dir -> Pos
step (x,y) = \case
  E -> (x+1,y)
  W -> (x-1,y)
  NE -> (x,y+1)
  NW -> (x-1,y+1)
  SE -> (x+1,y-1)
  SW -> (x,y-1)

type Pos = (Int,Int)

data Dir = E | W | NE | NW | SE | SW deriving Show

gram :: Par [[Dir]]
gram = separated nl line
  where
    line = some dir
    dir = alts
      [ do lit 'e'; pure E
      , do lit 'w'; pure W
      , do lit 'n'; alts [ do lit 'e'; pure NE
                         , do lit 'w'; pure NW ]
      , do lit 's'; alts [ do lit 'e'; pure SE
                         , do lit 'w'; pure SW ]
      ]
