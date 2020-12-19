
module Day19 (main) where

import Control.Applicative (many)
import Misc (check)
import Par4 (Par,parse,separated,nl,int,key,alts,lit,sp)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day19.input.sam"
  inp <- parse gram <$> readFile "input/day19.input"
  let !_ = check 2 $ part1 sam
  print ("day19, part1", check 265 $ part1 inp)

  samb <- parse gram <$> readFile "input/day19.input.samb"
  let !_ = check 3 $ part1 samb
  let !_ = check 12 $ part2 samb
  print ("day19, part2", check 394 $ part2 inp)


part1 :: Input -> Int
part1 Input{rules,messages} = do
  let matcher = buildMatcher rules
  length [ m | m <- messages, isMatch matcher m ]

part2 :: Input -> Int
part2 Input{rules,messages} = do
  let rules' = [ rule | rule@(n,_) <- rules, n/=8 && n/=11 ] ++ replacements
  let matcher = buildMatcher rules'
  length [ m | m <- messages, isMatch matcher m ]
  where
    replacements =
      [ (8, Alt (Seq [42]) (Seq [42,8]))
      , (11, Alt (Seq [42,31]) (Seq [42,11,31])) ]


isMatch :: Matcher -> Message -> Bool
isMatch matcher m = any (== []) (runMatcher matcher m)

data Matcher = Matcher { runMatcher :: [Let] -> [[Let]] }

buildMatcher :: [Rule] -> Matcher
buildMatcher rules = look 0
  where
    look :: Int -> Matcher
    look n = Map.findWithDefault (error $ "look: " ++ show n) n m
      where m = Map.fromList [ (n, matchRhs rhs) | (n,rhs) <- rules ]

    matchRhs :: Rhs -> Matcher
    matchRhs = \case
      Base x -> matchLet x
      Seq xs -> foldr matchSeq matchOk [ look x | x <- xs ]
      Alt x1 x2 -> matchAlt (matchRhs x1) (matchRhs x2)

matchLet :: Let -> Matcher
matchLet x = Matcher $ \case
  y:rest -> if (x==y) then [rest] else []
  [] -> []

matchOk :: Matcher
matchOk = Matcher $ \xs -> [xs]

matchSeq :: Matcher -> Matcher -> Matcher
matchSeq m1 m2 = Matcher $ \xs -> [zs | ys <- runMatcher m1 xs, zs <- runMatcher m2 ys]

matchAlt :: Matcher -> Matcher -> Matcher
matchAlt m1 m2 = Matcher $ \xs -> runMatcher m1 xs ++ runMatcher m2 xs


data Input = Input { rules :: [Rule], messages :: [Message] } deriving Show
type Rule = (Id,Rhs)
type Id = Int
data Rhs = Base Let | Seq [Id] | Alt Rhs Rhs deriving Show
data Let = A | B deriving (Eq,Show)
type Message = [Let]

gram :: Par Input
gram = do
  rules <- terminated nl rule
  messages <- separated nl line
  pure $ Input { rules, messages }
  where
    rule :: Par Rule
    rule = do
      n <- int
      key ":"
      r <- rhs []
      pure (n,r)

    line = many ab
    ab = alts [ do lit 'a'; pure A, do lit 'b'; pure B]

    rhs :: [Int] -> Par Rhs
    rhs acc = do
      alts
        [ pure $ Seq (reverse acc)
        , do
            sp;
            alts [ do lit '"'; x <- ab; lit '"'; pure $ Base x
                 , do x <- int; rhs (x:acc)
                 , do
                     key "| "
                     r2 <- separated sp int
                     pure $ Alt (Seq (reverse acc)) (Seq r2)
                 ]
        ]

terminated :: Par () -> Par a -> Par [a]
terminated term p = many (do x <- p; term; pure x)
