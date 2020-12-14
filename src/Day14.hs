
module Day14 (main) where

import Control.Applicative (many)
import Data.Bits (bit, (.|.), (.&.), complement)
import Data.Map (Map)
import Misc (check)
import Par (Par,parse,separated,nl,int,lit,alts,key)
import qualified Data.Map.Strict as Map (empty,elems,insert)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day14.input.sam"
  samb <- parse gram <$> readFile "input/day14.input.samb"
  inp <- parse gram <$> readFile "input/day14.input"
  let !_ = check 165 $ part1 sam
  print ("day14, part1", check 7611244640053 $ part1 inp)
  let !_ = check 208 $ part2 samb
  print ("day14, part2", check 3705162613854 $ part2 inp)

data Instruction = Mask [Bit] | Write Int Int deriving Show
data Bit = One | Zero | X deriving Show

gram :: Par [Instruction]
gram = separated nl line where
  line = do lit 'm'; alts [mask,write]
  mask = do
    key "ask = "
    Mask <$> many bit
  write = do
    key "em["
    addr <- int
    key "] = "
    val <- int
    pure $ Write addr val
  bit = alts
    [ do lit '0'; pure Zero
    , do lit '1'; pure One
    , do lit 'X'; pure X
    ]

data State = State
  { mem :: Map Int Int
  , ao :: (Int,Int)
  }

part1 :: [Instruction] -> Int
part1 = loop State { mem = Map.empty, ao = (complement 0,0) }
  where
    loop :: State -> [Instruction] -> Int
    loop state@State{mem,ao=(a,o)} = \case
      [] -> sum [ v | v <- Map.elems mem ]
      Write addr val : more -> do
        let maskedVal = (val .&. a) .|. o
        loop state { mem = Map.insert addr maskedVal mem } more
      Mask mask : more -> do
        loop state { ao = aoFromMask mask } more

aoFromMask :: [Bit] -> (Int,Int)
aoFromMask bits = do
  let a = complement $ foldl (.|.) 0 [ bit i | (i,Zero) <- zip [0..] (reverse bits) ]
  let o =              foldl (.|.) 0 [ bit i | (i,One)  <- zip [0..] (reverse bits) ]
  (a, o)


data State2 = State2
  { mem :: Map Int Int
  , mask :: [Bit]
  }

part2 :: [Instruction] -> Int
part2 = loop State2 { mem = Map.empty, mask = take 36 (repeat Zero) }
  where
    loop :: State2 -> [Instruction] -> Int
    loop state@State2{mem,mask} = \case
      [] -> sum [ v | v <- Map.elems mem ]
      Write addr val : more -> do
        let as = expandAddrs addr mask
        loop state { mem = foldl write mem as } more
          where
            write :: Map Int Int -> Int -> Map Int Int
            write mem a = Map.insert a val mem
      Mask mask : more -> do
        loop state { mask } more

expandAddrs :: Int -> [Bit] -> [Int]
expandAddrs x0 mask = expand x0 (zip [0..] (reverse mask))
  where
    expand x = \case
      [] -> [x]
      (_,Zero):xs -> expand x xs
      (i,One):xs -> expand (bit i .|. x) xs
      (i,X):xs -> expand (complement (bit i) .&. x) xs ++ expand (bit i .|. x) xs
