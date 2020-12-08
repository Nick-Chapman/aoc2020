
module Day5 (main) where

import Control.Applicative (some)
import Misc (check)
import Par (Par,parse,nl,alts,lit)

main :: IO ()
main = do
  let !_ = check (70*8 + 7) $ parse line "BFFFBBFRRR"
  s <- readFile "input/day5.input"
  let xs = parse file s
  print ("day5, part1", check 826 $ maximum xs)
  print ("day5, part2", check [678] [c | c <- [minimum xs.. maximum xs], c `notElem` xs])

file :: Par [Int]
file = separated nl line

line :: Par Int
line = do
  r <- mkBin <$> some (alts [do lit 'F'; pure 0, do lit 'B'; pure 1])
  c <- mkBin <$> some (alts [do lit 'L'; pure 0, do lit 'R'; pure 1])
  pure $ r * 8 + c
    where mkBin = foldl (\acc d -> 2*acc + d) 0

separated :: Par () -> Par a -> Par [a]
separated sep p = do x <- p; alts [ pure [x], do sep; xs <- separated sep p; pure (x:xs) ]
