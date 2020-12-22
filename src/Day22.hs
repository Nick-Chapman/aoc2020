
module Day22 (main) where

import Data.Set (Set)
import Misc (check)
import ParE (Par,parse,separated,nl,key,int)
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day22.input.sam"
  inp <- parse gram <$> readFile "input/day22.input"
  let !_ = check 306 $ part1 sam
  print ("day22, part1", check 33010 $ part1 inp)
  let !_ = check 291 $ part2 sam
  print ("day22, part2", check 32769 $ part2 inp) -- 7 seconds!


part1 :: Game -> Int
part1 = loop
  where
    loop g =
      case over g of
        Nothing -> loop (step g)
        Just player -> scoreP player g

    step :: Game -> Game
    step Game{p1,p2} =
      case (p1,p2) of
        ([],_) -> error "step,p1=[]"
        (_,[]) -> error "step,p2=[]"
        (x1:p1,x2:p2) -> do
          case winner of
            P1 -> Game {p1 = p1 ++ [x1,x2], p2}
            P2 -> Game {p2 = p2 ++ [x2,x1], p1}
          where
            winner = if (x1 > x2) then P1 else P2


part2 :: Game -> Int
part2 = play Set.empty
  where
    play :: Set Game -> Game -> Int
    play visited g = do
      if g `elem` visited then scoreP P1 g else
        case over g of
          Nothing -> play (Set.insert g visited) (step g)
          Just player -> scoreP player g

    playRec :: Set Game -> Game -> Player
    playRec visited g = do
      if g `elem` visited then P1 else
        case over g of
          Nothing -> playRec (Set.insert g visited) (step g)
          Just player -> player

    step :: Game -> Game
    step Game{p1,p2} =
      case (p1,p2) of
        ([],_) -> error "step,p1=[]"
        (_,[]) -> error "step,p2=[]"
        (x1:p1,x2:p2) -> do
          case winner of
            P1 -> Game {p1 = p1 ++ [x1,x2], p2}
            P2 -> Game {p2 = p2 ++ [x2,x1], p1}
          where
            winner =
              if length p1 >= x1 && length p2 >= x2
              then do
                let subg = Game { p1 = take x1 p1, p2 = take x2 p2 }
                playRec Set.empty subg
              else
                if (x1 > x2) then P1 else P2



data Player = P1 | P2

over :: Game -> Maybe Player
over Game{p1,p2} =
  if p1==[] then Just P2 else
    if p2==[] then Just P1 else
      Nothing

scoreP :: Player -> Game -> Int
scoreP player Game{p1,p2} = do
  let xs = case player of P1 -> p1; P2 -> p2
  sum [i*x | (i,x) <- zip [1..] (reverse xs) ]

data Game = Game {p1 :: [Int], p2 :: [Int]} deriving (Eq,Ord,Show)

gram :: Par Game
gram = do
  key "Player 1:"; nl
  p1 <- separated nl int
  nl;nl
  key "Player 2:"; nl
  p2 <- separated nl int
  pure Game {p1,p2}
