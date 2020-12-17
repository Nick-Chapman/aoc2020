
module Day17 (main) where

import Data.Set (Set)
import GHC.Int (Int32)
import Misc (check)
import Prelude hiding (cycle)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- readFile "input/day17.input.sam"
  inp <- readFile "input/day17.input"
  let !_= check 112 $ part1 sam
  print ("day17, part1", check 310 $ part1 inp)
  let !_= check 848 $ part2 sam
  print ("day17, part2", check 2056 $ part2 inp)

part1 :: String -> Int
part1 = simulate init3 (step link3)
  where init3 (x,y) = (x,y,0)

part2 :: String -> Int
part2 = simulate init4 (step link4)
  where init4 (x,y) = (x,y,0,0)

simulate :: Ord a => ((Int32,Int32) -> a) -> (Set a -> Set a) -> String -> Int
simulate init step str = loop (0::Int) (parse init str) where
  loop cycle active = do
    if cycle == 6 then Set.size active else
      loop (cycle+1) (step active)

parse :: Ord a => ((Int32,Int32) -> a) -> String -> Set a
parse init s = Set.fromList
  [ init (x,y)
  | (y,line) <- zip [1..] (lines s)
  , (x,char) <- zip [1..] line
  , case char of '#' -> True; '.' -> False; _ -> undefined ]

step :: Ord a => (a -> [a]) -> Set a -> Set a
step link active = do
  let m = Map.fromListWith (+)
        [ (pos',1::Int32)
        | pos <- Set.toList active
        , pos' <- link pos ]
  Set.fromList
    [ p | (p,count) <- Map.toList m
    , count == 3 || (count == 2 && p `elem` active) ]

type I3 = (Int32,Int32,Int32)

link3 :: I3 -> [I3]
link3 (a,b,c) = [ (a+x,b+y,c+z) | x <- tri, y <- tri, z <- tri, x/=0||y/=0||z/=0 ]
  where tri = [-1,0,1]

type I4 = (Int32,Int32,Int32,Int32)

link4 :: I4 -> [I4]
link4 (a,b,c,d) = [ (a+x,b+y,c+z,d+w) | x <- tri, y <- tri, z <- tri, w <- tri, x/=0||y/=0||z/=0||w/=0 ]
  where tri = [-1,0,1]
