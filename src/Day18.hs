
module Day18 (main) where

import Control.Applicative (many)
import Misc (check)
import ParE (Par,parse,separated,nl,alts,int,sp,lit)

main :: IO ()
main = do
  sam <- readFile "input/day18.input.sam"
  inp <- readFile "input/day18.input"
  let !_ = check 13632 $ part1 sam
  print ("day18, part1", check 75592527415659 $ part1 inp)
  let !_ = check 23340 $ part2 sam
  print ("day18, part2", check 360029542265462 $ part2 inp)

part1 :: String -> Integer
part1 s = sum (map eval (parse gram1 s))

part2 :: String -> Integer
part2 s = sum (map eval (parse gram2 s))

eval :: Exp -> Integer
eval = \case
  Num n -> n
  Add e1 e2 -> eval e1 + eval e2
  Mul e1 e2 -> eval e1 * eval e2

data Exp = Num Integer | Add Exp Exp | Mul Exp Exp
  deriving Show

gram1 :: Par [Exp]
gram1 = separated nl exp
  where
    atom :: Par Exp
    atom = alts [ (Num . fromIntegral) <$> int , bracketed exp ]

    exp :: Par Exp
    exp = do
      x <- atom
      xs <- many $ do
        sp
        f <- alts [ do lit '+'; pure Add
                  , do lit '*'; pure Mul ]
        sp
        x <- atom
        pure (f,x)

      pure $ foldl node x xs
        where node e1 (f,e2) = f e1 e2

gram2 :: Par [Exp]
gram2 = separated nl exp
  where
    atom :: Par Exp
    atom = alts [ (Num . fromIntegral) <$> int , bracketed exp ]

    thing :: Par Exp
    thing = do
      x <- atom
      xs <- many $ do
        sp
        f <- do lit '+'; pure Add
        sp
        x <- atom
        pure (f,x)

      pure $ foldl node x xs
        where node e1 (f,e2) = f e1 e2

    exp :: Par Exp
    exp = do
      x <- thing
      xs <- many $ do
        sp
        f <- do lit '*'; pure Mul
        sp
        x <- thing
        pure (f,x)

      pure $ foldl node x xs
        where node e1 (f,e2) = f e1 e2

bracketed :: Par a -> Par a
bracketed p = do
  lit '('
  x <- p
  lit ')'
  pure x
