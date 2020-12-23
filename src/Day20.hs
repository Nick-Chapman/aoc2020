
module Day20 (main) where

import Control.Applicative (some)
import Data.List (transpose)
import Misc (check)
import ParE (Par,parse,separated,nl,key,int,alts,lit)
import Prelude hiding (flip)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day20.input.sam"
  inp <- parse gram <$> readFile "input/day20.input"
  let !_ = check 20899048083289 $ part1 sam
  print ("day20, part1", check 23497974998093 $ part1 inp)

  --sequence_ [part2 3 sam True n | n <- [1,0,2,3]]
  --part2 12 inp False 0
  --_old_part2 3 sam True 0

  --sequence_ [part2 3 n sam | n <- [0..3]]
  --sequence_ [part2 12 n inp | n <- [0..3]]

  let !_ = check 273 $ part2 3 sam
  print ("day20, part2", check 2256 $ part2 12 inp)


part1 :: [Tile] -> Integer
part1 tiles = do
  let xs = [ (b,loc) | tile <- tiles, (loc,b) <- locatedBordersTile tile ]
  let ys = collate xs
  let zs = collate [ (id,(b,loc)) | (b,[loc@Loc{id,flipped=False}]) <- ys ]
  product [ n | (n,[_,_]) <- zs ]


_old_part2 :: Int -> [Tile] ->  Bool -> Int -> IO ()
_old_part2 width tiles overallFlipped cornerN = do
  putStrLn "--------------------------------------------------"
  print ("old-part2",width,overallFlipped,cornerN)
  let ys = collate [ (b,loc) | tile <- tiles, (loc,b) <- locatedBordersTile tile ]
  let exterior = [ (b,loc) | (b,[loc]) <- ys ]
  let interior = [ (b,loc1,loc2) | (b,[loc1,loc2]) <- ys ]
  let exterior1 = collate [ (id,(b,loc)) | (b,loc@Loc{id,flipped=False}) <- exterior ]
  let corners = [ (n,xs) | (n,xs@[_,_]) <- exterior1 ]
  --mapM_ print corners
  let (corner1id,_) = corners !! cornerN -- any should do, pick this to match 3x3 in instructions
  let [e1,e2] = [ edge
                | (_,loc1,loc2) <- interior
                , Loc{id,edge,flipped} <- [loc1,loc2]
                , flipped == overallFlipped
                , id==corner1id
                ]
  --print (e1,e2)
  let corner1rot = subE R $ head [ a | (a,b) <- [(e1,e2),(e2,e1)], addE a R == b ]
  --print corner1rot
  let dub = [ x | (_,a,b) <- interior , x <- [(a,b),(b,a)]]
  --mapM_ print interior
  -- 1,2,3
  -- 4,5,6
  -- 7,8,9
  let rot1 = corner1rot
  let id1 = corner1id
  let flip1 = overallFlipped
  let place1 = (id1,rot1,flip1)
  print place1
  --print ("row1",findLine dub place1 R width)
  let col = findLine dub place1 B width
  let solve = [ findLine dub place R width | place <- col ]
  mapM_ print solve
  _imageX <- readFile "input/day20.input.sam.expected-image"
  --putStrLn _imageX
  let image :: Image = arrange tiles solve
  --putStrLn _image
  let orientations = [ (edge,flip) | edge <- [T,R,B,L], flip <- [True,False] ]
  --print $ (_imageX == show image)
  print $ [ _imageX == show (rotate r (flip b image)) | (r,b) <- orientations ]
  print [ countMonsters (rotate r (flip b image)) | (r,b) <- orientations ]
  return ()



countMonsters :: Image -> Int
countMonsters (Image xss) = loop1 xss
  where
    loop1 xss =
      case xss of
        [] -> 0
        _:tail -> loop2 xss + loop1 tail

    loop2 xss =
      case head xss of
        [] -> 0
        _:_ ->
          (if matchMonster xss then 1 else 0)
          + loop2 (map tail xss)


matchMonster :: [[Cell]] -> Bool
matchMonster = \case
  (    _:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:o:_:_)
    : (g:_:_:_:_:h:i:_:_:_:_:j:k:_:_:_:_:l:m:n:_)
    : (_:a:_:_:b:_:_:c:_:_:d:_:_:e:_:_:f:_:_:_:_)
    : _
    ->
    all (==On) [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]
  _
    -> False

type Placement = (Integer,Edge,Bool)

findLine :: [(Loc,Loc)] -> Placement -> Edge -> Int -> [Placement]
findLine dubs place dir n =
  if n == 0 then [] else
    place : findLine dubs (findLink dubs place dir) dir (n-1)


{-
("part2",3,True,0)
(1171,T,True)
[(1171,T,True),(1489,B,True),(2971,B,True)]
[(2473,L,True),(1427,B,True),(2729,B,True)]
[(3079,B,False),(2311,B,True),(1951,B,True)]
-}

findLink :: [(Loc,Loc)] -> Placement -> Edge -> Placement
findLink dub (lastId,lastRot,lastFlip) direction = do
  let x = case [ x | x@(Loc{id,edge,flipped},_) <- dub
                 , id == lastId
                 , flipped == lastFlip
                 , edge == addE lastRot direction
                 -- , edge == subE direction lastRot
                 ]
          of
            [x] -> x
            _:_:_ ->
              error (show ("head>2",lastId,lastRot,direction) ++ "\n" ++ unlines (map show info))
                where info =
                        [ x | x@(Loc{id,edge,flipped},_) <- dub
                          , let _ = (id,edge,flipped)
                          , id == lastId
                          , flipped == lastFlip
                          --, edge == addE lastRot direction
                          --, edge == subE direction lastRot
                          ]
            [] -> do
              --print (lastId,lastFlip)
              error (show ("head0",lastId,lastRot,direction) ++ "\n" ++ unlines (map show info))
                where info =
                        [ x | x@(Loc{id,edge,flipped},_) <- dub
                          , let _ = (id,edge,flipped)
                          , id == lastId
                          , flipped == lastFlip
                          --, edge == addE lastRot direction
                          --, edge == subE direction lastRot
                          ]
  let (Loc{},Loc{id=linkId,edge,flipped}) = x
  --let linkRot = addE lastRot (subE (invE direction) edge)
  let linkRot = subE (invE direction) edge
  --let linkRot = subE  (_oppE direction) edge
  let linkFlip = if (flipped /= lastFlip) then lastFlip else not lastFlip
  let linkPlace = (linkId,linkRot,linkFlip)
  linkPlace

arrange :: [Tile] -> [[Placement]] -> Image
arrange tiles pss = do
  let iss :: [[Image]] = [ [arrange1 tiles p | p <- ps ] | ps <- pss ]
  above [ beside is | is <- iss ]

above :: [Image] -> Image
above xs = Image $ concat (map unImage xs)

beside :: [Image] -> Image
beside = \case
  [] -> error "beside[]"
  x:xs -> foldl beside2 x xs

beside2 :: Image -> Image -> Image
beside2 (Image lines1) (Image lines2) =
  Image [ line1 ++ line2 | (line1,line2) <- zip lines1 lines2 ]

arrange1 :: [Tile] -> Placement -> Image
arrange1 tiles (idK,r,b) = do
  let image = head [ image | Tile{image,id} <- tiles, id==idK ]
  rotate r (flip b (trim image))

trim :: Image -> Image
trim (Image lines) =
  Image [ (init . tail) line | line <- (init . tail) lines ]

rotate :: Edge -> Image -> Image
rotate = \case
  T -> \x -> x
  R -> rotateR
  B -> rotateR . rotateR
  L -> rotateR . rotateR . rotateR

rotateR :: Image -> Image
rotateR (Image xss) = Image (reverse (transpose xss))

flip :: Bool -> Image -> Image
flip = \case
  False -> \x -> x
  True -> \(Image xs) -> Image (reverse xs)



invE :: Edge -> Edge
invE = \case T -> T; R -> L; B -> B; L -> R

_oppE :: Edge -> Edge
_oppE = \case T -> B; R -> L; B -> T; L -> R

subE :: Edge -> Edge -> Edge
subE e1 e2 = addE e1 (invE e2)

addE :: Edge -> Edge -> Edge
addE = \case
  T -> \e -> e
  R -> \case T -> R; R -> B; B -> L; L -> T
  B -> \case T -> B; R -> L; B -> T; L -> R
  L -> \case T -> L; R -> T; B -> R; L -> B



collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])


data Loc = Loc { id :: Integer, edge :: Edge, flipped :: Bool }
data Edge = T | L | B | R deriving (Eq,Ord,Show)
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
newtype Image = Image { unImage :: [[Cell]] }
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

----------------------------------------------------------------------
-- part2, reworked!

part2 :: Int -> [Tile] -> Int
part2 size tiles = do
  let cornerN = 0
  --putStrLn "--------------------------------------------------"
  --print ("part2",cornerN)

  let bs = [ (border,(id,edge)) | Tile{id,image} <- tiles, (edge,border) <- bordersTile image ]

  let matches = [ (x1,x2,False)
                | (b1,x1@(id1,_)) <- bs
                , (b2,x2@(id2,_)) <- bs
                , b1 == b2
                , id1 /= id2
                ]

  let flippedMatches = [ (x1,x2,True)
                       | (b1,x1@(id1,_)) <- bs
                       , let rev_b1 = revBorder b1
                       , (b2,x2@(id2,_)) <- bs
                       , rev_b1 == b2
                       , id1 /= id2
                       ]

  let joins = (matches ++ flippedMatches)

  let b = True

  let p1 :: Placement =
        [ (id,rotationForTopLeftCorner b e1 e2, b)
        | (id,[e1,e2]) <- collate [ (id,edge) | ((id,edge),_,_) <- joins ]
        ] !! cornerN

  let adjacent :: Edge -> Placement -> Placement
      adjacent dir placeK = do
        let (idK,rK,bK) = placeK
        let edgeK = edgeWhichSitsAt (rK,bK) dir
        let (_,(id2,e2),b2) = the [ m
                                  | m@((id1,edge1),_,_) <- joins
                                  , id1 == idK, edge1 == edgeK
                                  ]
        let b = bK == b2
        let rot = the [ r | r <- [T,R,B,L], addE r (flipE b e2) == _oppE dir ]
        let place' :: Placement = (id2,rot,b)
        place'


  let solve0 = [ take size (iterate (adjacent R) p)
              | p <- take size (iterate (adjacent B) p1) ]

  let solve = map (map hack) solve0
        where hack (id,r,b) = (id,transposeE r,b)
  --mapM_ print solve

  let image :: Image = arrange tiles solve
  let orientations = [ (edge,flip) | edge <- [T,R,B,L], flip <- [True,False] ]

  --_imageX <- readFile "input/day20.input.sam.expected-image"
  --print $ [ _imageX == show (rotate r (flip b image)) | (r,b) <- orientations ]
  --print [ countMonsters (rotate r (flip b image)) | (r,b) <- orientations ]

  let m = the [ m
              | (r,b) <- orientations
              , let m = countMonsters (rotate r (flip b image))
              , m /= 0
              ]

  let pix = length [ () | On <- concat (unImage image) ]
  --print m
  --print pix
  let answer = pix - 15 * m
  --print answer
  answer

edgeWhichSitsAt :: (Edge,Bool) -> Edge -> Edge
edgeWhichSitsAt (r,b) goal =
  the [ e | e <- [T,R,B,L], addE r (flipE b e) == goal ]


flipE :: Bool -> Edge -> Edge -- not same as invE
flipE = \case
  False -> \e -> e
  True -> \case T -> B; R -> R; B -> T; L -> L

transposeE :: Edge -> Edge -- for HACK (flips L/R)
transposeE = \case T -> T; R -> L; B -> B; L -> R

the :: [a] -> a
the = \case
  [] -> error "the[]"
  [x] -> x
  _:_:_ -> error "the>=2"

rotationForTopLeftCorner :: Bool -> Edge -> Edge -> Edge
rotationForTopLeftCorner b e1 e2 = rot
  where
    rot =
      head [ r
           | r <- [T,R,B,L]
           , let x1 = addE r (flipE b e1)
           , let x2 = addE r (flipE b e2)
           , (x1,x2) == (R,B) || (x1,x2) == (B,R)
           ]

bordersTile :: Image -> [(Edge,Border)]
bordersTile (Image grid) =
  [ (T, Border (head grid))
  , (L, Border (reverse (map head grid)))
  , (B, Border (reverse (last grid)))
  , (R, Border (map last grid))
  ]

revBorder :: Border -> Border
revBorder (Border xs) = Border (reverse xs)
