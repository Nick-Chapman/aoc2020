
module Par(Par,parse,many0,int,key,char,ident,space,nl,ws0,ws1) where

import Control.Monad (ap,liftM)

import qualified EarleyM as EM (
  Gram,Lang,fail,alts,getToken,parse,
  Parsing(..),ParseError(..),SyntaxError(..),Ambiguity(..),Pos)
import qualified Data.Char as Char

instance Functor Par where fmap = liftM
instance Applicative Par where pure = return; (<*>) = ap
instance Monad Par where return = Ret; (>>=) = Bind

parse :: Show a => Par a -> String -> a
parse par string = runLG string $ langOfPar par

ident :: Par String
ident = do x <- alpha; xs <- many0 alpha; return (x : xs)
  where alpha = sat Char.isAlpha

ws0 :: Par ()
ws0 = skipWhile space

ws1 :: Par ()
ws1 = do space; ws0

int :: Par Int
int = do
  d0 <- digit
  ds <- many0 digit
  return $ foldl (\acc d -> 10*acc + d) d0 ds

skipWhile :: Par () -> Par ()
skipWhile p = do _ <- many0 p; return ()

many0 :: Par a -> Par [a]
many0 g = many_g
  where many_g = Alts [return [], do x <- g; xs <- many_g; return (x : xs)]

key :: String -> Par ()
key cs = mapM_ symbol cs
  where symbol x = do t <- Token; if t==x then return () else Fail

digit :: Par Int
digit = digit
  where
     digit = do c <- sat Char.isDigit; return (digitOfChar c)
     digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

space :: Par ()
space = do _ <- sat Char.isSpace; return ()

nl :: Par ()
nl = do _ <- sat (== '\n'); return ()

sat :: (Char -> Bool) -> Par Char
sat pred = do c <- Token; if pred c then return c else Fail

char :: Par Char
char = Token

data Par a where
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Token :: Par Char
  Alts :: [Par a] -> Par a

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
