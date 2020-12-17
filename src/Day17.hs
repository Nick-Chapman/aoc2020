
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
part1 str = loop 0 (parse3 str) where
  loop :: Int -> Set I3 -> Int
  loop cycle active = do
    if cycle == 6 then Set.size active else
      loop (cycle+1) (step3 active)

part2 :: String -> Int
part2 str = loop 0 (parse4 str) where
  loop :: Int -> Set I4 -> Int
  loop cycle active = do
    if cycle == 6 then Set.size active else
      loop (cycle+1) (step4 active)


type I3 = (Int32,Int32,Int32)

delta3 :: [I3]
delta3 = [ (x,y,z) | x <- tri, y <- tri, z <- tri, x/=0||y/=0||z/=0 ]
  where tri = [-1,0,1]

add3 :: I3 -> I3 -> I3
add3 (a,b,c) (x,y,z) = (a+x,b+y,c+z)

step3 :: Set I3 -> Set I3
step3 active = do
  let m = Map.fromListWith (+)
        [ (add3 pos del,1::Int32)
        | pos <- Set.toList active
        , del <- delta3 ]
  Set.fromList
    [ p | (p,count) <- Map.toList m
    , count == 3 || (count == 2 && p `elem` active) ]

parse3 :: String -> Set I3
parse3 s = Set.fromList
  [ (x,y,0)
  | (y,line) <- zip [1..] (lines s)
  , (x,char) <- zip [1..] line
  , case char of '#' -> True; '.' -> False; _ -> undefined ]


type I4 = (Int32,Int32,Int32,Int32)

delta4 :: [I4]
delta4 = [ (x,y,z,w) | x <- tri, y <- tri, z <- tri, w <- tri, x/=0||y/=0||z/=0||w/=0 ]
  where tri = [-1,0,1]

add4 :: I4 -> I4 -> I4
add4 (a,b,c,d) (x,y,z,w) = (a+x,b+y,c+z,d+w)

step4 :: Set I4 -> Set I4
step4 active = do
  let m = Map.fromListWith (+)
        [ (add4 pos del,1::Int32)
        | pos <- Set.toList active
        , del <- delta4 ]
  Set.fromList
    [ p | (p,count) <- Map.toList m
    , count == 3 || (count == 2 && p `elem` active) ]

parse4 :: String -> Set I4
parse4 s = Set.fromList
  [ (x,y,0,0)
  | (y,line) <- zip [1..] (lines s)
  , (x,char) <- zip [1..] line
  , case char of '#' -> True; '.' -> False; _ -> undefined ]
