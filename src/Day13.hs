
module Day13 (main) where

import Data.List
import Data.Ord
import Misc (check)
import Par (Par,parse,separated,nl,int,lit,alts)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day13.input.sam"
  inp <- parse gram <$> readFile "input/day13.input"
  let !_ = check 295 $ part1 sam
  print ("day13, part1", check 3269 $ part1 inp)
  let !_ = check 3417 $ part2 [Just 17,Nothing,Just 13,Just 19]
  let !_ = check 1068781 $ part2 (busses sam)
  print ("day13, part1",  check 672754131923874 $ part2 (busses inp))

part1 :: Input -> Int
part1 Input{now,busses} =
  uncurry (*) $ minimumBy (comparing snd) [ (b, b - now `mod` b) | Just b <- busses ]

data Equ = Equ { d :: Int, r :: Int }

part2 :: [Maybe Int] -> Int
part2 xs = steps [ Equ {d, r = (d-i) `mod` d} | (i,Just d) <- zip [0..] xs ]
  where
    steps :: [Equ] -> Int
    steps = \case
      [] -> 0
      Equ{d=d0,r=r0}:xs -> do
        r0 + d0 * steps [ Equ{d,r=r1}
                        | Equ{d,r} <- xs
                        , let r1 = mDivide ((r-r0) `mod` d) d0 d ]

-- modulo division: returns a/b (m)
-- i.e. if: mDivide a b m == n, then: n*b == i*m+a for some i
mDivide :: Int -> Int -> Int -> Int
mDivide a b m = loop 0 0 where
  loop n r = if r == a then n else loop (n+1) ((r + b) `mod` m)

data Input = Input { now :: Int, busses :: [Maybe Int] }

gram :: Par Input
gram = do
  now <- int
  nl;
  busses <- separated (lit ',') item
  pure $ Input {now,busses}
    where item = alts [ do lit 'x'; pure Nothing , Just <$> int]
