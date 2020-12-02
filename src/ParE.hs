
-- | Earley Parser Combinators
module ParE (Par,parse,int,many,lit,key,char,word,ws0,ws1,sp,nl) where

import Control.Monad (ap,liftM)
import qualified Data.Char as Char

import qualified EarleyM as EM (
  Gram,Lang,fail,alts,getToken,parse,
  Parsing(..),ParseError(..),SyntaxError(..),Ambiguity(..),Pos)

instance Functor Par where fmap = liftM
instance Applicative Par where pure = Ret; (<*>) = ap
instance Monad Par where (>>=) = Bind

digit :: Par Int
word :: Par String
key :: String -> Par ()
int :: Par Int
ws0 :: Par ()
ws1 :: Par ()
skipWhile :: Par () -> Par ()
many :: Par a -> Par [a]
sp :: Par ()
nl :: Par ()
lit :: Char -> Par ()
sat :: (Char -> Bool) -> Par Char
char :: Par Char

word = do x <- alpha; xs <- many alpha; return (x : xs) where alpha = sat Char.isAlpha

int = do
  d0 <- digit
  ds <- many digit
  return $ foldl (\acc d -> 10*acc + d) d0 ds

ws1 = do sp; ws0
ws0 = skipWhile sp

skipWhile p = do _ <- many p; return ()
many g = Alts [return [], do x <- g; xs <- many g; return (x : xs)]

digit = do c <- sat Char.isDigit; return (digitOfChar c)

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

sp = lit ' '
nl = lit '\n'
lit x = do _ <- sat (== x); pure ()
sat pred = do c <- Token; if pred c then return c else Fail
key cs = mapM_ lit cs
char = Token

data Par a where
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Token :: Par Char
  Alts :: [Par a] -> Par a

parse :: Show a => Par a -> String -> a
parse par string = runLG string $ langOfPar par

withTok :: EM.Gram Char -> Par a -> EM.Gram a
withTok tok = conv
  where
    conv :: Par a -> EM.Gram a
    conv = \case
      Ret a -> return a
      Bind p f -> conv p >>= conv . f
      Fail -> EM.fail
      Token -> tok
      Alts ps -> EM.alts (map conv ps)

langOfPar :: Par a -> EM.Lang Char (EM.Gram a)
langOfPar par = do
  tok <- EM.getToken
  return $ withTok tok par

runLG :: Show a => String -> EM.Lang Char (EM.Gram a) -> a
runLG s lang =
  case EM.parse lang s of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> error $ prettyPE pe
      Right a -> a
  where

    prettyPE :: EM.ParseError -> String
    prettyPE = \case
      EM.AmbiguityError (EM.Ambiguity tag p1 p2) -> show ("Ambiguity",tag,p1,p2)
      EM.SyntaxError se -> prettySE se

    prettySE :: EM.SyntaxError -> String
    prettySE = \case
      EM.UnexpectedTokenAt pos -> "unexpected " ++ show (s!!(pos-1)) ++ " at " ++ lc (pos-1)
      EM.UnexpectedEOF{} -> "unexpected EOF"
      EM.ExpectedEOF pos -> "expected EOF at " ++ lc (pos-1)

    lc :: EM.Pos -> String
    lc p = "line: " ++ show line ++ ", col: " ++ show col
      where
        line :: Int = 1 + length [ () | c <- take p s, c == '\n' ]
        col :: Int = length (takeWhile (/= '\n') (reverse (take p s)))
