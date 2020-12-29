
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
  let !_ = check 273 $ part2 3 sam
  print ("day20, part2", check 2256 $ part2 12 inp)


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
      image <- Image <$> separated nl (some cell)
      pure $ Tile {image,id}

    cell :: Par Cell
    cell = alts [ do lit '#'; pure On
                , do lit '.'; pure Off ]


data Border = Border [Cell] deriving (Eq,Ord)
instance Show Border where show (Border xs) = xs >>= show

revBorder :: Border -> Border
revBorder (Border xs) = Border (reverse xs)


data Dir = N | E | S | W deriving (Eq,Ord,Show)

flipE :: Bool -> Dir -> Dir
flipE = \case
  False -> \e -> e
  True -> \case N -> S; E -> E; S -> N; W -> W

transposeE :: Dir -> Dir -- for HACK (flips W/E)
transposeE = \case N -> N; E -> W; S -> S; W -> E

oppE :: Dir -> Dir
oppE = \case N -> S; E -> W; S -> N; W -> E

addE :: Dir -> Dir -> Dir
addE = \case
  N -> \e -> e
  E -> \case N -> E; E -> S; S -> W; W -> N
  S -> \case N -> S; E -> W; S -> N; W -> E
  W -> \case N -> W; E -> N; S -> E; W -> S

rotationForTopLeftCorner :: Bool -> Dir -> Dir -> Dir
rotationForTopLeftCorner b e1 e2 =
  the [ r
      | r <- [N,E,S,W]
      , let x1 = addE r (flipE b e1)
      , let x2 = addE r (flipE b e2)
      , (x1,x2) == (E,S) || (x1,x2) == (S,E)
      ]

edgeWhichSitsAt :: (Dir,Bool) -> Dir -> Dir
edgeWhichSitsAt (r,b) goal =
  the [ e | e <- [N,E,S,W], addE r (flipE b e) == goal ]


part1 :: [Tile] -> Integer
part1 tiles = do
  let xs = collate [ (b',id) | Tile{id,image} <- tiles
                             , (_,b) <- bordersTile image
                             , b' <- [b, revBorder b]
                             ]
  let zs = collate [ (id,()) | (_,[id]) <- xs ]
  product [ id | (id,[_,_,_,_]) <- zs ]


collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])


part2 :: Int -> [Tile] -> Int
part2 size tiles = do
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
  let p1 :: Placement =
        [ (id,rotationForTopLeftCorner b e1 e2, b)
        | (id,[e1,e2]) <- collate [ (id,edge) | ((id,edge),_,_) <- joins ]
        ] !! cornerN
        where (cornerN,b) = (0,True)

  let adjacent :: Dir -> Placement -> Placement
      adjacent dir placeK = do
        let (idK,rK,bK) = placeK
        let edgeK = edgeWhichSitsAt (rK,bK) dir
        let (_,(id2,e2),b2) = the [ m
                                  | m@((id1,edge1),_,_) <- joins
                                  , id1 == idK, edge1 == edgeK
                                  ]
        let b = bK == b2
        let rot = the [ r | r <- [N,E,S,W], addE r (flipE b e2) == oppE dir ]
        let place' :: Placement = (id2,rot,b)
        place'

  let solve0 = [ take size (iterate (adjacent E) p)
              | p <- take size (iterate (adjacent S) p1) ]

  let solve = map (map hack) solve0
        where hack (id,r,b) = (id,transposeE r,b)

  let image :: Image = arrange tiles solve
  let orientations = [ (edge,flip) | edge <- [N,E,S,W], flip <- [True,False] ]

  let m = the [ m
              | (r,b) <- orientations
              , let m = countMonsters (rotate r (flip b image))
              , m /= 0
              ]

  let pix = length [ () | On <- concat (unImage image) ]
  let answer = pix - 15 * m
  answer


bordersTile :: Image -> [(Dir,Border)]
bordersTile (Image grid) =
  [ (N, Border (head grid))
  , (E, Border (map last grid))
  , (S, Border (reverse (last grid)))
  , (W, Border (reverse (map head grid)))
  ]


type Placement = (Integer,Dir,Bool)


arrange :: [Tile] -> [[Placement]] -> Image
arrange tiles pss = do
  let iss :: [[Image]] = [ [arrange1 tiles p | p <- ps ] | ps <- pss ]
  above [ beside is | is <- iss ]

arrange1 :: [Tile] -> Placement -> Image
arrange1 tiles (idK,r,b) = do
  let image = head [ image | Tile{image,id} <- tiles, id==idK ]
  rotate r (flip b (trim image))

above :: [Image] -> Image
above xs = Image $ concat (map unImage xs)

beside :: [Image] -> Image
beside = \case
  [] -> error "beside[]"
  x:xs -> foldl beside2 x xs

beside2 :: Image -> Image -> Image
beside2 (Image lines1) (Image lines2) =
  Image [ line1 ++ line2 | (line1,line2) <- zip lines1 lines2 ]

trim :: Image -> Image
trim (Image lines) =
  Image [ (init . tail) line | line <- (init . tail) lines ]

flip :: Bool -> Image -> Image
flip = \case
  False -> \x -> x
  True -> \(Image xs) -> Image (reverse xs)

rotate :: Dir -> Image -> Image
rotate = \case
  N -> \x -> x
  E -> \(Image xss) -> Image (reverse (transpose xss))
  S -> \(Image xss) -> Image (map reverse (reverse xss))
  W -> \(Image xss) -> Image (map reverse (transpose xss))


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


the :: [a] -> a
the = \case
  [] -> error "the[]"
  [x] -> x
  _:_:_ -> error "the>=2"
