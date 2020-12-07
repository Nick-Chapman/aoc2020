
module Day7 (main) where

import Data.Map (Map)
import Data.Set (Set, union)
import Misc (check)
import Par4 as Par (Par,parse,nl,sp,key,lit,word,int,alts)
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
part1 rules = Set.size $ searchReach step (step (Bag "shiny" "gold"))
  where
    mayBePackedIn :: Map Bag [Bag]
    mayBePackedIn = Map.fromListWith (++) [ (i,[o]) | Rule o ins <- rules , (_,i) <- ins]
    step :: Bag ->  [Bag]
    step b = Map.findWithDefault [] b mayBePackedIn


searchReach :: (Bag -> [Bag]) -> [Bag] -> Set Bag
searchReach step initial = loop 0 Set.empty (Set.fromList initial)
  where
    loop :: Int -> Set Bag -> Set Bag -> Set Bag
    loop n acc frontierSet = do
      let frontier = Set.toList frontierSet
      if frontier == [] then acc else do
        let acc' = acc `union` Set.fromList frontier
        let frontier' = Set.fromList [ a2 | a1 <- frontier, a2 <- step a1, a2 `notElem` acc' ]
        loop (n+1) acc' frontier'


part2 :: [Rule] -> Int
part2 rules = count (Bag "shiny" "gold") - 1
  where
    containsMap = Map.fromList [ (o,is) | Rule o is <- rules ]
    contains b = Map.findWithDefault (error (show ("count", b))) b containsMap
    count b = do 1 + sum [ n * count b | (n,b) <- contains b ]


data Rule = Rule Bag [(Int,Bag)] deriving Show
data Bag = Bag Shade Colour deriving (Eq,Ord,Show)
type Shade = String
type Colour = String

gram :: Par [Rule]
gram = separated nl line
  where
    line :: Par Rule
    line = do
      b <- bag
      sp
      key "contain"
      sp
      gs <- alts [separated (key ", ") nbag, nothing]
      lit '.'
      pure $ Rule b gs

    nothing = do
      key "no other bags"
      pure []

    nbag = do
      n <- int
      sp
      b <- bag
      pure (n,b)

    bag = do
      s <- word
      sp
      c <- word
      sp
      key "bag"
      alts [lit 's', pure ()]
      pure $ Bag s c

separated :: Par () -> Par a -> Par [a]
separated sep p = do x <- p; alts [ pure [x], do sep; xs <- separated sep p; pure (x:xs) ]
