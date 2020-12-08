
module Day8 (main) where

import Data.Set (Set)
import Misc (check)
import Par4 as Par (Par,parse,nl,sp,lit,word,int,alts,separated)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day8.input.sam"
  inp <- parse gram <$> readFile "input/day8.input"
  let !_ = check (Loop 5) $ part1 sam
  print ("day8, part1",check (Loop 1709) $ part1 inp)
  let !_ = check [Complete 8] $ part2 sam
  print ("day8, part2", check [Complete 1976] $ part2 inp)

part1 :: [Op] -> Res
part1 = exec

part2 :: [Op] -> [Res]
part2 xs =
  [ res
  | i <- [0.. length xs - 1]
  , Just xs' <- pure $ fixup i xs
  , res@(Complete _) <- pure $ exec xs'
  ]
  where
    fixup :: Int -> [Op] -> Maybe [Op]
    fixup n xs = do
      let (front,op:back) = splitAt n xs
      op' <- fix1 op
      pure $ front ++ [op'] ++ back
        where
          fix1 :: Op -> Maybe Op
          fix1 = \case
            Nop n -> Just (Jmp n)
            Jmp n -> Just (Nop n)
            Acc _ -> Nothing

data Op
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving Show

data Res = Loop Int | Complete Int deriving (Eq,Show)

exec :: [Op] -> Res
exec ops = run Set.empty 0 0
  where
    size = length ops

    get :: Int -> Op
    get n = Map.findWithDefault (error (show ("get",n))) n m
      where
        m = Map.fromList (zip [0..] ops)

    run :: Set Int -> Int -> Int -> Res
    run visited a pc = do
      if pc `elem` visited
        then Loop a
        else if pc == size then Complete a
             else run' (Set.insert pc visited) a pc (get pc)

    run' visited a pc = \case
      Nop _ -> run visited a (pc+1)
      Acc n -> run visited (a+n) (pc+1)
      Jmp n -> run visited a (pc+n)

gram :: Par [Op]
gram = separated nl line
  where
    line = do
      f <- opcode
      sp
      n <- signed
      pure $ f n

    opcode :: Par (Int -> Op)
    opcode = word >>= \case
      "nop" -> pure Nop
      "acc" -> pure Acc
      "jmp" -> pure Jmp
      _ -> alts[]

    signed :: Par Int
    signed = do
      sign <- alts [ do lit '+'; pure id , do lit '-'; pure negate]
      n <- int
      pure $ sign n
