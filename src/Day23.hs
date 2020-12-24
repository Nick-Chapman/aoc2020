
module Day23 (main) where

import Misc (check)

main :: IO ()
main = do
  let sam = "389125467"
  let inp = "962713854"
  let !_ = check "92658374" $ part1 10 sam
  let !_ = check "67384529" $ part1 100 sam
  print ("day23, part1", check "65432978" $ part1 100 inp)

  -- part2. too slow: needs to -> 1mil!
  let res@(a,b,_) = part2 1000 sam -- ~1sec
  let !_ = check (524,822) (a,b)
  print ("day 23, part2 (1k)", res)
  --print ("day 23, part2 (2k)", part2 2000 sam) -- ~2sec


part2 :: Int -> String -> (Int,Int,String)
part2 sizeL xs = do
  let n = 10 * sizeL
  finalize2 (iterate step1 (init2 xs sizeL) !! n)
  where
    init2 :: String -> Int -> Circle
    init2 string sizeL = do
      let xs0 = [ read [c] | c <- string ]
      let xs1 = [ length xs0 + 1 .. sizeL ]
      makeCircle (xs0 ++ xs1)

    finalize2 :: Circle -> (Int,Int,String)
    finalize2 c = do
      let Circle{onRight,stepsL,stepsR,shiftsLR,shiftsRL} = searchR 1 c
      let a:b:_ = onRight
      (a,b, show (('L',stepsL,shiftsRL), ('R',stepsR,shiftsLR)))


part1 :: Int -> String -> String
part1 n xs = finalize1 (iterate step1 (init1 xs) !! n)
  where
    init1 :: String -> Circle
    init1 string = makeCircle [ read [c] | c <- string ]

    finalize1 :: Circle -> String
    finalize1 c = do
      let Circle{onLeft,onRight} = searchR 1 c
      concat (map show (onRight ++ reverse onLeft))


step1 :: Circle -> Circle
step1 c1@Circle{size} = do
  let orig = onPoint c1
  let (c2,seg) = cutR3 c1
  let target = findTarget seg orig
  let (c3,returnSearch) = search target c2
  let c4 = insertR seg c3
  let c5 = returnSearch orig c4
  let c6 = right c5
  c6
  where
    findTarget :: Seg -> Int -> Int
    findTarget (x1,x2,x3) orig = loopDec orig
      where
        loopDec n = do
          let n1 = dec n
          if n1 == x1 || n1 == x2 || n1 == x3 then loopDec n1 else n1
        dec :: Int -> Int
        dec n = do
          let n1 = n - 1
          if n1==0 then size else n1


data Circle = Circle
  { size :: Int
  , onLeft :: [Int]
  , onPoint :: Int
  , onRight :: [Int]
  , stepsL :: Int
  , stepsR :: Int
  , shiftsLR :: Int
  , shiftsRL :: Int
  }

instance Show Circle where
  show Circle{onLeft,onPoint,onRight} =
    concat $
    (map show (reverse onLeft))
    ++ ["(" ++ show onPoint ++ ")"]
    ++ (map show onRight)

makeCircle :: [Int] -> Circle
makeCircle = \case
  [] -> error "makeCircle[]"
  x:xs ->
    Circle { size = 1 + length xs
           , onLeft = [], onPoint = x, onRight = xs
           , stepsL = 0, stepsR = 0, shiftsLR = 0, shiftsRL = 0 }

type Seg = (Int,Int,Int)

cutR3 :: Circle -> (Circle, Seg)
cutR3 c@Circle{onRight} =
  case onRight of
    x1:x2:x3:xs -> (c {onRight = xs}, (x1,x2,x3))
    _ ->
      cutR3 (shiftLR c)

insertR :: Seg -> Circle -> Circle
insertR (x1,x2,x3) c@Circle{onRight} = c { onRight = x1:x2:x3:onRight }

search :: Int -> Circle -> (Circle, Int -> Circle -> Circle)
search e c =
  if onPoint c == e then error "search" else loop (left c) (right c)
  where
    loop c1 c2 =
      if onPoint c1 == e then (c1, searchR) else
        if onPoint c2 == e then (c2, searchL) else
          loop (left c1) (right c2)

searchR :: Int -> Circle -> Circle
searchR e c = if onPoint c == e then c else searchR e (right c)

searchL :: Int -> Circle -> Circle
searchL e c = if onPoint c == e then c else searchL e (left c)

right :: Circle -> Circle
right c@Circle{onLeft=l,onPoint=x,onRight=r,stepsR} =
  case r of
    x':r' -> c {onLeft = x : l, onPoint = x', onRight = r' , stepsR = stepsR + 1}
    [] -> right (shiftLR c)

left :: Circle -> Circle
left c@Circle{onLeft=l,onPoint=x,onRight=r,stepsL} =
  case l of
    x':l' -> c {onLeft = l', onPoint = x', onRight = x : r , stepsL = stepsL + 1}
    [] -> left (shiftRL c)

shiftLR :: Circle -> Circle
shiftLR c@Circle{size,onLeft=l,onRight=r,shiftsLR} = do
  let (l',xs) = splitAt (size `div` 2) l
  c {onLeft=l',onRight = r ++ reverse xs, shiftsLR = shiftsLR + 1}

shiftRL :: Circle -> Circle
shiftRL c@Circle{size,onLeft=l,onRight=r,shiftsRL} = do
  let (r',xs) = splitAt (size `div` 2) r
  c {onLeft=l ++ reverse xs ,onRight = r', shiftsRL = shiftsRL + 1}
