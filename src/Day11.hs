
module Day11 (main) where

import Control.Applicative (many)
import Data.Map (Map)
import Data.Set (Set,member)
import Misc (check)
import Par (Par,parse,separated,nl,alts,lit)
import qualified Data.Map as Map
import qualified Data.Set as Set (size,fromList)


main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day11.input.sam"
  inp <- parse gram <$> readFile "input/day11.input"
  let !_ = check 37 $ sim step1 sam
  print ("day11, part1", check 2448 $ sim step1 inp)
  let !_ = check 26 $ sim step2 sam
  print ("day11, part2", check 2234 $ sim step2 inp)


type Pos = (Int,Int)

newtype Grid = Grid [[Cell]] deriving (Eq)
data Cell = Floor | Seat | Occ deriving (Eq)

instance Show Cell where
  show = \case Floor -> "."; Seat -> "L"; Occ -> "#"

instance Show Grid where
  show (Grid grid) = unlines [ concat [ show cell | cell <- line ] | line <- grid ]

gram :: Par Grid
gram = Grid <$> separated nl line
  where line = many cell
        cell = alts [do lit 'L'; pure Seat, do lit '.'; pure Floor]


step1 :: Grid -> Grid
step1 grid = mapGrid f grid
  where

    isOcc :: Pos -> Bool
    isOcc pos =  pos `member` occ
      where
        occ :: Set Pos
        occ = occSet grid

    f pos = \case
      Floor -> Floor
      Seat -> case occNeighbors of [] -> Occ; _:_ -> Seat -- ==0
      Occ -> case occNeighbors of _:_:_:_:_ -> Seat; _ -> Occ -- >=4
      where
        occNeighbors = [ () | pos' <- neighbors pos, isOcc pos' ]


step2 :: Grid -> Grid
step2 grid = mapGrid f grid
  where

    canSeeOcc :: Pos -> Pos -> Bool
    canSeeOcc pos dir = case Map.lookup pos m of
      Nothing -> False
      Just Seat -> False
      Just Occ -> True
      Just Floor -> canSeeOcc (move pos dir) dir
      where
        m :: Map Pos Cell
        m = gridMap grid

    f pos = \case
      Floor -> Floor
      Seat -> case occNeighbors of [] -> Occ; _:_ -> Seat -- ==0
      Occ -> case occNeighbors of _:_:_:_:_:_ -> Seat; _ -> Occ -- >=5
      where
        occNeighbors = [ () | dir <- directions, canSeeOcc (move pos dir) dir ]

    move (x,y) (dx,dy) = (x+dx,y+dy)


sim :: (Grid -> Grid) -> Grid -> Int
sim step grid = loop grid (step grid)
  where
    loop g1 g2 = do
      if g1 == g2
        then countOcc g2
        else loop g2 (step g2)

_sim :: (Grid -> Grid) -> Grid -> IO Int
_sim step grid = loop (0::Int) grid (step grid)
  where
    loop n g1 g2 = do
      print ("step",n)
      print g2
      if g1 == g2
        then pure $ countOcc g2
        else loop (n+1) g2 (step g2)


countOcc :: Grid -> Int
countOcc = Set.size . occSet

mapGrid :: ((Int,Int) -> Cell -> Cell) -> Grid -> Grid
mapGrid f (Grid grid) =
  Grid [ [ f (x,y) cell | (x,cell) <- zip [0..] line ] | (y,line) <- zip [0..] grid ]

occSet :: Grid -> Set Pos
occSet (Grid grid) = Set.fromList
  [ (x,y)
  | (y,line) <- zip [0..] grid
  , (x,cell) <- zip [0..] line
  , cell == Occ
  ]

gridMap :: Grid -> Map Pos Cell
gridMap (Grid grid) = Map.fromList
  [ ((x,y),cell)
  | (y,line) <- zip [0..] grid
  , (x,cell) <- zip [0..] line
  ]

neighbors :: Pos -> [Pos]
neighbors (x,y) = [ (x+dx,y+dy) | (dx,dy) <- directions ]

directions :: [Pos]
directions = [ (dx,dy) | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0) ]
