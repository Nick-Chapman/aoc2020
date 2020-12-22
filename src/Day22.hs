
module Day22 (main) where

import Misc (check)
import ParE (Par,parse,separated,nl,key,int)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day22.input.sam"
  inp <- parse gram <$> readFile "input/day22.input"
  let !_ = check 306 $ part1 sam
  print ("day22, part1", check 33010 $ part1 inp)

part1 :: Game -> Int
part1 = loop
  where
    loop g = do if over g then score g else loop (step g)

    step :: Game -> Game
    step Game{p1,p2} =
      case (p1,p2) of
        ([],_) -> error "step,p1=[]"
        (_,[]) -> error "step,p2=[]"
        (x1:p1,x2:p2) -> do
          if x1 > x2
            then Game {p1 = p1 ++ [x1,x2],p2}
            else Game {p2 = p2 ++ [x2,x1],p1}

over :: Game -> Bool
over Game{p1,p2} = p1==[] || p2==[]

score :: Game -> Int
score Game{p1,p2} = do
  let xs = if p1 == [] then p2 else p1
  sum [i*x | (i,x) <- zip [1..] (reverse xs) ]

data Game = Game {p1 :: [Int], p2 :: [Int]} deriving Show

gram :: Par Game
gram = do
  key "Player 1:"; nl
  p1 <- separated nl int
  nl;nl
  key "Player 2:"; nl
  p2 <- separated nl int
  pure Game {p1,p2}
