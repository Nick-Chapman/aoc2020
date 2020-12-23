
module Day23 (main) where

import Misc (check)

main :: IO ()
main = do
  let sam = "389125467"
  let inp = "962713854"
  let !_ = check "92658374" $ part1 10 sam
  let !_ = check "67384529" $ part1 100 sam
  print ("day23, part1", check "65432978" $ part1 100 inp)

part1 :: Int -> String -> String
part1 n xs = finalize (iterate step xs !! n)

finalize :: String -> String
finalize xs = do
  let (before,_:after) = span (/='1') xs
  after ++ before

step :: String -> String
step = \case
  x:a:b:c:xs -> do
    let next :: Char = head [ n | n <- dropWhile (>x) looped, n `notElem` [x,a,b,c] ]
    let (before,_:after) = span (/=next) xs
    (before ++ [next] ++ [a,b,c] ++ after ++ [x])
  _ ->
    undefined

looped :: String
looped = concat (repeat "987654321")

