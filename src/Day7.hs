
module Day7 (main) where

import Data.Map (Map)
import Misc (check,bfs)
import Par4 as Par (Par,parse,nl,sp,key,lit,word,int,alts,opt,separated)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- readFile "input/day7.input.sam"
  samb <- readFile "input/day7.input.samb"
  s <- readFile "input/day7.input"
  let !_ = check 4 $ part1 (parse gram sam)
  print ("day7, part1", check 161 $ part1 (parse gram s))
  let !_ = check 32 $ part2 (parse gram sam)
  let !_ = check 126 $ part2 (parse gram samb)
  print ("day7, part2", check 30899 (part2 (parse gram s)))

part1 :: [Rule] -> Int
part1 rules = Set.size $ bfs step (step shinyGold)
  where
    step :: Bag -> [Bag]
    step b = Map.findWithDefault [] b mayPackIn
      where
        mayPackIn :: Map Bag [Bag]
        mayPackIn = Map.fromListWith (++) [ (i,[o]) | Rule o ins <- rules , (_,i) <- ins]

part2 :: [Rule] -> Int
part2 rules = count shinyGold - 1
  where
    count b = do 1 + sum [ n * count b | (n,b) <- contains b ]
    contains b = Map.findWithDefault (error (show ("count", b))) b containsMap
      where
        containsMap = Map.fromList [ (o,is) | Rule o is <- rules ]

data Rule = Rule Bag [(Int,Bag)] deriving Show
data Bag = Bag (String,String) deriving (Eq,Ord,Show)

shinyGold :: Bag
shinyGold = Bag ("shiny","gold")

gram :: Par [Rule]
gram = separated nl line
  where
    line = do
      b <- bag
      key " contain "
      gs <- alts [ separated (key ", ") $ do n <- int; sp; b <- bag; pure (n,b)
                 , do key "no other bags"; pure [] ]
      lit '.'
      pure $ Rule b gs

    bag = do s <- word; sp; c <- word; key " bag"; _ <- opt (lit 's'); pure $ Bag (s,c)
