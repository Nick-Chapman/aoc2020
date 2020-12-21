
module Day21 (main) where

import Data.List (nub,sortBy,intercalate)
import Data.Map (Map)
import Data.Ord (comparing)
import Misc (check)
import ParE (Par,parse,separated,nl,word,key,sp,lit)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day21.input.sam"
  inp <- parse gram <$> readFile "input/day21.input"
  let !_ = check 5 $ part1 sam
  print ("day21, part1", check 2635 $ part1 inp)
  let !_ = check "mxmxvkd,sqjhc,fvjkl" $ part2 sam
  let expected = "xncgqbcp,frkmp,qhqs,qnhjhn,dhsnxr,rzrktx,ntflq,lgnhmx"
  print ("day21, part2", check expected $ part2 inp)

part1 :: [Food] -> Int
part1 foods = do
  let allA :: [A] = nub $ concat [ as | (_,as) <- foods ]
  let allI :: [I] = nub $ concat [ is | (is,_) <- foods ]
  let iByA :: [ (A,[[I]]) ] = collate [ (a,is) | (is,as) <- foods, a <- as ]
  let iByAm :: Map A [[I]] = Map.fromList iByA
  let
    dontContain :: I -> A -> Bool
    dontContain i a = do
      let iss = Map.findWithDefault (error "dontContain") a iByAm
      any (\is -> i `notElem` is) iss
  let safeI i = all (dontContain i) allA
  let safe :: [I] = [ i | i <- allI, safeI i ]
  length [ i | (is,_) <- foods, i <- is, i `elem` safe ]


part2 :: [Food] -> String
part2 foods = do
  let allA :: [A] = nub $ concat [ as | (_,as) <- foods ]
  let allI :: [I] = nub $ concat [ is | (is,_) <- foods ]
  let iByA :: [ (A,[[I]]) ] = collate [ (a,is) | (is,as) <- foods, a <- as ]
  let iByAm :: Map A [[I]] = Map.fromList iByA
  let
    dontContain :: I -> A -> Bool
    dontContain i a = do
      let iss = Map.findWithDefault (error "dontContain") a iByAm
      any (\is -> i `notElem` is) iss
  let safeI i = all (dontContain i) allA
  let safe :: [I] = [ i | i <- allI, safeI i ]
  let danger :: [I] = filter (not . (`elem` safe)) allI
  let
    mightContain :: I -> [A]
    mightContain i = [ a | a <- allA,  not (dontContain i a) ]
  let loop :: [(I,[A])] -> [(A,I)]
      loop = \case
        [] -> []
        cands -> do
          let (i,[aAssigned]) = head [ c | c@(_,as) <- cands, length as == 1 ]
          let cands' = [ (i,as')
                       | (i,as) <- cands
                       , let as' = filter (/=aAssigned) as
                       , as' /= [] ]
          (aAssigned,i) : loop cands'
  let cand0 :: [(I,[A])] = [ (i, mightContain i) | i <- danger ]
  let sortedAssigned = sortBy (comparing fst) (loop cand0)
  intercalate "," (map (unI . snd) sortedAssigned)


collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])

newtype A = A String deriving (Eq,Ord,Show)
newtype I = I { unI :: String } deriving (Eq,Ord,Show)

type Food = ([I],[A])

gram :: Par [Food]
gram = separated nl line
  where
    line :: Par Food
    line = do
      is <- map I <$> separated sp word
      key " (contains "
      as <- map A <$> separated (do lit ','; sp) word
      lit ')'
      pure (is,as)
