
module Day20 (main) where

import Control.Applicative (some)
import Misc (check)
import ParE (Par,parse,separated,nl,key,int,alts,lit)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day20.input.sam"
  inp <- parse gram <$> readFile "input/day20.input"
  --mapM_ print sam
  let !_ = check 20899048083289 $ part1 sam
  print ("day20, part1", check 23497974998093 $ part1 inp)


part1 :: [Tile] -> Integer
part1 tiles = do
  let xs = [ (b,loc) | tile <- tiles, (loc,b) <- locatedBordersTile tile ]
  --mapM_ print xs
  let ys = collate xs
  --mapM_ print ys
  let zs = collate [ (id,(b,loc)) | (b,[loc@Loc{id,flipped=False}]) <- ys ]
  --mapM_ print $ zs
  --mapM_ print [ (n,xs) | (n,xs@[_,_]) <- zs ]
  product [ n | (n,[_,_]) <- zs ]


collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])


data Loc = Loc { id :: Integer, edge :: Edge, flipped :: Bool }
data Edge = T | B | L | R deriving Show

instance Show Loc where show Loc{id,edge,flipped} = show (id,edge,flipped)

data Border = Border [Cell] deriving (Eq,Ord)
instance Show Border where show (Border xs) = xs >>= show

locatedBordersTile :: Tile -> [(Loc,Border)]
locatedBordersTile Tile{id,image=Image grid} =
  [ (Loc id T False, Border (head grid))
  , (Loc id B False, Border (reverse (last grid)))
  , (Loc id L False, Border (reverse (map head grid)))
  , (Loc id R False, Border (map last grid))

  , (Loc id T True, Border (head grid'))
  , (Loc id B True, Border (reverse (last grid')))
  , (Loc id L True, Border (reverse (map head grid')))
  , (Loc id R True, Border (map last grid'))
  ]
  where grid' = reverse grid


data Tile = Tile { image :: Image, id :: Integer }
newtype Image = Image [[Cell]]
data Cell = On | Off deriving (Eq,Ord)

instance Show Tile where
  show Tile{image,id} =
    unlines ["Tile " ++ show id ++ ":", show image]

instance Show Image where
  show (Image grid) = unlines [ line >>= show | line <- grid ]

instance Show Cell where show = \case On -> "#"; Off -> "."

gram :: Par [Tile]
gram = separated (do nl; nl) tile
  where
    tile = do
      key "Tile "
      id <- fromIntegral <$> int
      key ":\n"
      image <- Image <$> separated nl line
      pure $ Tile {image,id}

    line :: Par [Cell]
    line = some cell

    cell :: Par Cell
    cell = alts [ do lit '#'; pure On
                , do lit '.'; pure Off ]
