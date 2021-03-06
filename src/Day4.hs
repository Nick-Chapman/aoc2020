
module Day4 (main) where

import Control.Applicative (many,some)
import Misc (check)
import Par (Par,parse,alts,lit,word,sat,nl,sp)
import qualified Data.Char as Char (isNumber,isAlpha)

main :: IO ()
main = do
  s <- readFile "input/day4.input"
  let passports = parse gram s
  let part1 =
        [ p
        | p@(Pass kvs) <- passports
        , let n = length kvs
        , let keys = map (\(KV k _) -> k) kvs
        , let hasCid = Cid `elem` keys
        , (n==8) || ((n==7) && not hasCid)
        ]
  print ("day4, part1", check 192 $ length part1)
  let part2 =
        [ p
        | p@(Pass kvs) <- part1
        , all validate kvs
        ]
  print ("day4, part2", check 101 $ length part2)

data Pass = Pass [KV]
  deriving Show

data KV = KV Field String
  deriving Show

data Field = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid
  deriving (Eq,Show)

{-
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
-}

validate :: KV -> Bool
validate (KV k v) = case k of
  Byr -> do
    let n::Int = read v
    length v == 4 && n >= 1920 && n <= 2002
  Iyr -> do
    let n::Int = read v
    length v == 4 && n >= 2010 && n <= 2020
  Eyr -> do
    let n::Int = read v
    length v == 4 && n >= 2020 && n <= 2030
  Hgt -> do
    let u = (reverse . take 2 . reverse) v
    let v' = (reverse . tail . tail . reverse) v
    case u of
      "cm" -> do
        let n::Int = read v'
        n >= 150 && n <= 193
      "in" -> do
        let n::Int = read v'
        n >= 59 && n <= 76
      _ -> False
  Hcl -> do
    length v == 7 && head v == '#' && all (`elem` "0123456789abcdef")  (tail v)
  Ecl -> do
    v `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
  Pid -> do
    length v == 9 && all Char.isNumber v
  Cid -> do
    True



gram :: Par [Pass]
gram = separated nl passport
  where
    passport :: Par Pass
    passport = (Pass . concat) <$> many line

    line :: Par [KV]
    line = do
      xs <- separated (alts [sp]) kv
      nl
      pure xs

    kv = do
      k <- field
      lit ':'
      v <- word4
      pure $ KV k v

    word4 = some $ sat (\c -> Char.isAlpha c || Char.isNumber c || c == '#')

separated :: Par () -> Par a -> Par [a]
separated sep p = do
  x <- p
  alts [ pure [x]
       , do
           sep
           xs <- separated sep p
           pure $ x:xs
       ]

field :: Par Field
field = word >>= \case
  "byr" -> pure Byr
  "iyr" -> pure Iyr
  "eyr" -> pure Eyr
  "hgt" -> pure Hgt
  "hcl" -> pure Hcl
  "ecl" -> pure Ecl
  "pid" -> pure Pid
  "cid" -> pure Cid
  _ -> alts []
