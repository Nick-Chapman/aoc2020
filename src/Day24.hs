
module Day24 (main) where

import Control.Applicative (some)
import Data.Set (Set)
import Misc (check)
import Par (Par,parse,separated,nl,alts,lit)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day24.input.sam"
  inp <- parse gram <$> readFile "input/day24.input"
  let !_ = check 10 $ part1 sam
  print ("day24, part1", check 420 $ part1 inp)
  let !_ = check 2208 $ part2 sam
  print ("day24, part2", check 4206 $ part2 inp)

part1 :: [[Dir]] -> Int
part1 input = do
  let ps = [ foldl step (0,0) xs | xs <- input ]
  let m = Map.fromListWith (+) [ (p,1::Int) | p <- ps ]
  length [ c | c <- Map.elems m , c `mod` 2 == 1 ]

part2 :: [[Dir]] -> Int
part2 input = do
  let s0 = Set.fromList [ p | (p,c) <- Map.toList m , c `mod` 2 == 1 ]
        where
          ps = [ foldl step (0,0) xs | xs <- input ]
          m = Map.fromListWith (+) [ (p,1::Int) | p <- ps ]
  loop 0 s0
    where
      loop :: Int -> Set Pos -> Int
      loop n s = do
        if n == 100 then Set.size s else do
          loop (n+1) (gen s)

gen :: Set Pos -> Set Pos
gen black = do
  let m = Map.fromListWith (+) [ (step pos dir,1::Int)
                               | pos <- Set.toList black
                               , dir <- [E,W,NE,NW,SE,SW] ]
  Set.fromList
    [ p | (p,count) <- Map.toList m
    , count == 2 || (count == 1 && p `elem` black) ]

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
