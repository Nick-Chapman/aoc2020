
module Day23 (main) where

import Misc (check)
import Control.Monad.ST (ST,runST)
import Data.Array.ST (STUArray,newArray_,writeArray,readArray)

main :: IO ()
main = do
  let sam = "389125467"
  let inp = "962713854"
  let !_ = check "92658374" $ part1 10 sam
  let !_ = check "67384529" $ part1 100 sam
  print ("day23, part1", check "65432978" $ part1 100 inp)
  --let !_ = check 149245887792 $ part2 1000000 sam
  print ("day23, part2", check 287230227046 $ part2 1000000 inp) -- 50 seconds --> 0.9s

part1 :: Int -> String -> String
part1 n string = runST $ do
  let xs = [ read [c] | c <- string ]
  circle <- makeCircle xs
  _ <- loopM (step circle) n (head xs)
  finalize1 circle

finalize1 :: forall s. Circle s -> ST s String
finalize1 Circle{right} = do
  x <- right `readArray` 1
  collect [] x
  where
    collect :: [Int] -> Int -> ST s String
    collect acc a = do
      if a==1 then pure $ concat (map show (reverse acc)) else do
        x <- right `readArray` a
        collect (a:acc) x

part2 :: Int -> String -> Int
part2 sizeL string = runST $ do
  let xs = [ read [c] | c <- string ] ++ [ length string + 1 .. sizeL ]
  circle <- makeCircle xs
  _ <- loopM (step circle) (10*sizeL) (head xs)
  finalize2 circle

finalize2 :: forall s. Circle s -> ST s Int
finalize2 Circle{right} = do
  a <- right `readArray` 1
  b <- right `readArray` a
  pure $ a * b

loopM :: Monad m => Show a => (a -> m a) -> Int -> a -> m a
loopM f n x = if n == 0 then pure x else do
  let !n' = n-1
  x' <- f x
  loopM f n' x'

data Circle s = Circle { size :: Int, right :: STUArray s Int Int }

makeCircle :: [Int] -> ST s (Circle s)
makeCircle = \case
  [] -> error "makeCircle[]"
  x:xs -> do
    let size = length xs + 1
    right <- newArray_ (1,size)
    sequence_ [ writeArray right i e | (i,e) <- zip (x:xs) (xs ++ [x]) ]
    pure Circle {size, right}

step :: forall s. Circle s -> Int -> ST s Int
step Circle{size,right} point = do
  p1 <- right `readArray` point
  p2 <- right `readArray` p1
  p3 <- right `readArray` p2
  p4 <- right `readArray` p3
  let target = findTarget size (p1,p2,p3) point
  target1 <- right `readArray` target
  writeArray right target p1
  writeArray right p3 target1
  writeArray right point p4
  pure p4

findTarget :: Int -> (Int,Int,Int) -> Int -> Int
findTarget size (x1,x2,x3) orig = loopDec orig
  where
    loopDec n = do
      let n1 = dec n
      if n1 == x1 || n1 == x2 || n1 == x3 then loopDec n1 else n1
    dec :: Int -> Int
    dec n = do
      let n1 = n - 1
      if n1==0 then size else n1
