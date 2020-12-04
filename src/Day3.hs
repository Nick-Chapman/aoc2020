
module Day3 (main) where

import Control.Applicative (many)
import Misc (check)
import Par4 as Par (Par,alts,parse,lit,nl)
import qualified Data.Set as Set (fromList,member)

main :: IO ()
main = do
  s <- readFile "input/day3.input"
  let grid = parse gram s
  let (width,height) = (length (head grid), length grid)
  --print (width,height)

  let danger = Set.fromList
        [ (x,y) | (y,line) <- zip [0::Int ..] grid,  (x,cell) <- zip [0::Int ..] line, cell ]
  let at (x,y) = Set.member (x `mod` width, y) danger
  let onslope g = length [ (x,y) | y <- [0..height-1], let x = g * y, at (x,y) ]

  print ("day3, part1", check 268 (onslope 3))

  -- hack the single deep slope as a special case
  let deepSlope = length [ (x,y) | x <- [0 .. (height-1) `div` 2], let y = 2 * x, at (x,y)]
  let part2 = Prelude.foldl (*) 1 ([ onslope g | g <- [1,3,5,7] ] ++ [deepSlope])
  print ("day3, part2", check 3093068400 part2)

gram :: Par [[Bool]]
gram = many line
  where line = do xs <- many cell; nl; pure xs
        cell = alts [ do lit '#'; pure True
                    , do lit '.'; pure False ]
