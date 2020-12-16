
module Day16 (main) where

import Control.Applicative (many)
import Data.List (transpose,sortBy,isPrefixOf)
import Data.Ord (comparing)
import Misc (check)
import Par (Par,parse,separated,word,key,lit,int,sp,nl)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day16.input.sam"
  inp <- parse gram <$> readFile "input/day16.input"
  let !_ = check 71 $ part1 sam
  print ("day16, part1", check 21978 $ part1 inp)
  print ("day16, part2", check 1053686852011 $ part2 inp)

isValid :: Rule -> Int -> Bool
isValid (_,(a,b),(c,d)) x = (a<=x && x<=b) || (c<=x && x<=d)

part1 :: Input -> Int
part1 Input{rules,otherTickets} = do
  sum [ x | x <- concat otherTickets, not (any (\r -> isValid r x) rules) ]

part2 :: Input -> Int
part2 Input{rules,myTicket,otherTickets} = do
  let validOther = [ o | o@xs <- otherTickets , all (\x -> any (\r -> isValid r x) rules) xs ]

  let catagoryCandidates =
        [ (name,fields)
        | r@(name,_,_) <- rules
        , let fields = [ f
                       | (f,xs) <- zip [0::Int ..] (transpose validOther)
                       , all (isValid r) xs
                       ]
        ]
  let
    loop :: [(String,Int)] -> [(String,[Int])] -> [(String,Int)]
    loop acc = \case
      [] -> acc
      (cat,cands):more -> do
        let taken = map snd acc
        let available = [ cat | cat <- cands, cat `notElem` taken ]
        let [selected] = available -- partial match
        loop ((cat,selected):acc) more

  let assignedFields =
        loop [] (sortBy (comparing (length . snd)) catagoryCandidates)

  product
    [ myTicket !! f | (name,f) <- assignedFields, "departure" `isPrefixOf` name ]


data Input = Input { rules :: [Rule], myTicket :: Ticket, otherTickets :: [Ticket] }
type Rule = (String,Range,Range)
type Range = (Int,Int)
type Ticket = [Int]

gram :: Par Input
gram = input
  where
    input = do
      rules <- terminated nl rule
      nl
      key "your ticket:"
      nl
      myTicket <- ticket
      nl; nl
      key "nearby tickets:"
      nl
      otherTickets <- separated nl ticket
      pure $ Input { rules, myTicket, otherTickets }

    ticket = separated (lit ',') int

    rule = do
      w <- concat <$> separated sp word
      key ": "
      r1 <- range
      key " or "
      r2 <- range
      pure $ (w,r1,r2)

    range = do
      x <- int
      lit '-'
      y <- int
      pure (x,y)

terminated :: Par () -> Par a -> Par [a]
terminated term p = many (do x <- p; term; pure x)
