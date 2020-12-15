
module Day15 (main) where

import Control.Monad.ST (ST,runST)
import Data.Array.ST (STUArray,newArray,writeArray,readArray)
import Data.Map (Map)
import Misc (check)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let sam = [0,3,6]
  let !_ = check 436 $ search sam 2020

  --print $ check   7717 $ search sam     30_000
  --print $ check      5 $ search sam    300_000
  --print $ check   3240 $ search sam  3_000_000 -- (medium: approx 10s)
  --print $ check 175594 $ search sam 30_000_000 -- (medium: approx 5 min)

  let inp = [18,11,9,0,5,1]
  print ("day15, part1", check 959 $ search inp 2020)
  print ("day15, part2", check 116590 $ search inp 30_000_000) -- (fast: aprox 18s)

search :: [Int] -> Int -> Int
search = _fast

_fast :: [Int] -> Int -> Int
_fast start target = runST go
  where
    go :: ST s Int
    go = do
      a <- newArray (0,target) 0
      sequence_ [ writeArray a x i | (i,x) <- zip [1::Int ..] (init start) ]
      loop a (length start+1) (last start)

    loop :: STUArray s Int Int -> Int -> Int -> ST s Int
    loop a n h = do
      if n == (target+1) then pure h else do
        i <- readArray a h
        let v = if i == 0 then 0 else n-1-i
        writeArray a h (n-1)
        loop a (n+1) v


_medium :: [Int] -> Int -> Int
_medium start target = (start ++ loop (length start+1) (last start) m0) !! (target-1)
  where
    m0 = Map.fromList [ (x,i) | (i,x) <- zip [1..] (init start) ]
    loop :: Int -> Int -> Map Int Int -> [Int]
    loop n h m = do
      let v = case Map.lookup h m of Nothing -> 0; Just i -> (n-1-i)
      let m' = Map.insert h (n-1) m
      v : loop (n+1) v m'


_slow :: [Int] -> Int -> Int
_slow start target = (start ++ loop (length start) (reverse start)) !! (target-1)
  where
    loop n prev = do
      let h = head prev
      let xs = takeWhile (/=h) (tail prev)
      let i = length xs
      let v = if i == n-1 then 0 else i+1
      v : loop (n+1) (v:prev)
