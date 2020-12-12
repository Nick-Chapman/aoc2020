
module Day12 (main) where

import Misc (check)
import Par (Par,parse,separated,nl,alts,lit,int,key)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day12.input.sam"
  inp <- parse gram <$> readFile "input/day12.input"
  let !_ = check 25 $ part1 sam
  print ("day12, part1", check 1603 $ part1 inp)
  let !_ = check 286 $ part2 sam
  print ("day12, part2", check 52866 $ part2 inp)

data Step = Move Dir Int | Turn Hand Degree | Forward Int deriving Show
data Dir = N | S | E | W deriving Show
data Hand = L | R deriving Show
newtype Degree = Degree Int deriving Show

part1 :: [Step] -> Int
part1 = loop 0 0 E
  where
    loop :: Int -> Int -> Dir -> [Step] -> Int
    loop x y face = \case
      [] -> abs x + abs y
      step:steps -> case step of
        Forward n -> move n face
        Move dir n -> move n dir
        Turn hand (Degree d) ->
          loop x y (compN (turn1 hand) (d `div` 90) face) steps
        where
          move :: Int -> Dir -> Int
          move n = \case
            N -> loop x (y+n) face steps
            S -> loop x (y-n) face steps
            E -> loop (x+n) y face steps
            W -> loop (x-n) y face steps

    turn1 :: Hand -> Dir -> Dir
    turn1 = \case
      L -> \case N -> W; E -> N; S -> E; W -> S
      R -> \case N -> E; E -> S; S -> W; W -> N

part2 :: [Step] -> Int
part2 = loop (10,1) 0 0
  where
    loop :: (Int,Int) -> Int -> Int -> [Step] -> Int
    loop (wx,wy) x y xs = do
     case xs of
      [] -> abs x + abs y
      step:steps -> case step of
        Forward n -> loop (wx,wy) (x+n*wx) (y+n*wy) steps
        Move dir n -> move n dir
        Turn hand (Degree d) ->
          loop ((compN (turn2 hand) (d `div` 90)) (wx,wy)) x y steps
        where
          move :: Int -> Dir -> Int
          move n = \case
            N -> loop (wx,wy+n) x y steps
            S -> loop (wx,wy-n) x y steps
            E -> loop (wx+n,wy) x y steps
            W -> loop (wx-n,wy) x y steps

    turn2 :: Hand -> (Int,Int) -> (Int,Int)
    turn2 = \case
      L -> \(x,y) -> (-y,x)
      R -> \(x,y) -> (y,-x)

compN :: (a -> a) -> Int -> (a -> a)
compN f n = foldl (.) id $ take n (repeat f)

gram :: Par [Step]
gram = separated nl line
  where
    line = alts [move,turn,forward]
    forward = do lit 'F'; n <- int; pure $ Forward n
    move = do dir <- direction; n <- int; pure $ Move dir n
    turn = do h <- hand; d <- degree; pure $ Turn h d
    direction = alts
      [ do lit 'N'; pure N
      , do lit 'E'; pure E
      , do lit 'S'; pure S
      , do lit 'W'; pure W
      ]
    hand = alts
      [ do lit 'L'; pure L
      , do lit 'R'; pure R
      ]
    degree = alts
      [ do key "90"; pure $ Degree 90
      , do key "180"; pure $ Degree 180
      , do key "270"; pure $ Degree 270
      ]
