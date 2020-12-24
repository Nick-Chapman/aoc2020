
module Day23 (main) where

import Data.Map (Map)
import Misc (check)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let sam = "389125467"
  let inp = "962713854"
  let !_ = check "92658374" $ part1 10 sam
  let !_ = check "67384529" $ part1 100 sam
  print ("day23, part1", check "65432978" $ part1 100 inp)
  --let !_ = check 149245887792 $ part2 1000000 sam
  print ("day23, part1", check 287230227046 $ part2 1000000 inp) -- 50 seconds

part1 :: Int -> String -> String
part1 n xs = finalize1 (loop step n (init1 xs))
  where
    init1 :: String -> Circle
    init1 string = makeCircle [ read [c] | c <- string ]

part2 :: Int -> String -> Int
part2 sizeL xs = finalize2 $ loop step (10*sizeL) (init2 xs sizeL)
  where
    init2 :: String -> Int -> Circle
    init2 string sizeL = do
      let xs0 = [ read [c] | c <- string ]
      let xs1 = [ length xs0 + 1 .. sizeL ]
      makeCircle (xs0 ++ xs1)

loop :: (a -> a) -> Int -> a -> a
loop f n x = if n == 0 then x else do
  let! n' = n-1
  let! x' = f x
  loop f n' x'

data Circle = Circle
  { rightOf :: !(Map Int Int)
  , point :: !Int
  , size :: !Int
  }

finalize1 :: Circle -> String
finalize1 Circle{rightOf} = loop [] (right 1)
  where
    loop :: [Int] -> Int -> String
    loop acc a = do
      if a==1 then concat (map show (reverse acc)) else do
        loop (a:acc) (right a)

    right a = Map.findWithDefault (error $ "finalize1: " ++ show a) a rightOf

finalize2 :: Circle -> Int
finalize2 Circle{rightOf} = a * b
  where
    a = right 1
    b = right a
    right a = Map.findWithDefault (error $ "finalize2: " ++ show a) a rightOf

makeCircle :: [Int] -> Circle
makeCircle = \case
  [] -> error "makeCircle[]"
  x:xs -> Circle {point=x, rightOf, size = length xs + 1}
    where
      rightOf = Map.fromList (zip (x:xs) (xs++[x]))

step :: Circle -> Circle
step c@Circle{size,point,rightOf} = do
  let p1 = right point
  let p2 = right p1
  let p3 = right p2
  let p4 = right p3
  let target = findTarget (p1,p2,p3) point
  let target1 = right target
  let ups = [ setR target p1
            , setR p3 target1
            , setR point p4
            ]
  c { point = p4, rightOf = foldl (.) id ups rightOf}
  where
    right :: Int -> Int
    right n = Map.findWithDefault (error $ "right: " ++ show n) n rightOf

    setR a b = Map.insert a b

    findTarget :: (Int,Int,Int) -> Int -> Int
    findTarget (x1,x2,x3) orig = loopDec orig
      where
        loopDec n = do
          let n1 = dec n
          if n1 == x1 || n1 == x2 || n1 == x3 then loopDec n1 else n1
        dec :: Int -> Int
        dec n = do
          let n1 = n - 1
          if n1==0 then size else n1
