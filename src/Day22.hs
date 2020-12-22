
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
  where loop g = do if over g then score g else loop (step g)

data Game = Game
  { p1 :: [Int]
  , p2 :: [Int]
  , p1discard :: [Int]
  , p2discard :: [Int]
  }
  deriving Show

step :: Game -> Game
step g = if needRestack g then step (restack g) else do
  let Game{p1,p2,p1discard,p2discard} = g
  case (p1,p2) of
    ([],_) -> error "step,p1=[]"
    (_,[]) -> error "step,p2=[]"
    (x1:p1,x2:p2) -> do
      if x1 > x2
        then g {p1,p2, p1discard = x2:x1:p1discard}
        else g {p1,p2, p2discard = x1:x2:p2discard}

over :: Game -> Bool
over g = if needRestack g then over (restack g) else do
  let Game{p1,p2} = g
  p1==[] || p2==[]

score :: Game -> Int
score g = do
  let Game{p1,p2} = restack g
  let xs = if p1 == [] then p2 else p1
  sum [i*x | (i,x) <- zip [1..] (reverse xs) ]

needRestack :: Game -> Bool
needRestack Game{p1,p2,p1discard,p2discard} =
  (p1==[] && p1discard /= []) ||
  (p2==[] && p2discard /= [])

restack :: Game -> Game
restack Game{p1,p2,p1discard,p2discard} =
  Game
  { p1 = p1 ++ reverse p1discard
  , p2 = p2 ++ reverse p2discard
  , p1discard = []
  , p2discard = []
  }

gram :: Par Game
gram = do
  key "Player 1:"; nl
  p1 <- separated nl int
  nl;nl
  key "Player 2:"; nl
  p2 <- separated nl int
  pure Game {p1,p2,p1discard=[],p2discard=[]}
