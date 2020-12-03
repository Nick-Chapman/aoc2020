
-- | 4-value Parser Combinators
module Par4 (Par,parse,alts,int,many,lit,key,char,word,ws0,ws1,sp,nl) where

import Control.Monad (ap,liftM)
import qualified Data.Char as Char

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

alts :: [Par a] -> Par a
alts = foldl Alt Fail

word = do x <- alpha; xs <- many alpha; return (x : xs) where alpha = sat Char.isAlpha

int = do
  d0 <- digit
  ds <- many digit
  return $ foldl (\acc d -> 10*acc + d) d0 ds

ws1 = do sp; ws0
ws0 = skipWhile sp

skipWhile p = do _ <- many p; return ()
many g = Alt (return []) (do x <- g; xs <- many g; return (x : xs))

digit = do c <- sat Char.isDigit; return (digitOfChar c)

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

sp = lit ' '
nl = lit '\n'
lit x = do _ <- sat (== x); pure ()
key cs = mapM_ lit cs
char = sat (const True)
sat = Satisfy

data Par a where
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Satisfy :: (Char -> Bool) -> Par Char
  Alt :: Par a -> Par a -> Par a

type Res a = Either [Char] (a,[Char])

-- Four continuations:
data K4 a b = K4
  { eps :: a -> Res b            -- success; *no* input consumed
  , succ :: [Char] -> a -> Res b -- success; input consumed
  , fail :: () -> Res b          -- failure; *no* input consumed
  , err :: [Char] -> Res b       -- failure; input consumed (so an error!)
  }

parse :: Par a -> String -> a
parse parStart chars  = do

  case (run chars parStart kFinal) of
    Left remains -> error $ "failed to parse: " ++ report remains
    Right (a,[]) -> a
    Right (_,remains) -> error $ "unparsed input from: " ++ report remains

  where
    report :: String -> String
    report remains = item ++ " at " ++ lc pos
      where
        item = if pos == length chars then "<EOF>" else show (chars !! pos)
        pos = length chars - length remains

    lc :: Int -> String
    lc p = "line " ++ show line ++ ", column " ++ show col
      where
        line :: Int = 1 + length [ () | c <- take p chars, c == '\n' ]
        col :: Int = length (takeWhile (/= '\n') (reverse (take p chars)))

    kFinal = K4 { eps = \a -> Right (a,chars)
                , succ = \chars a -> Right (a,chars)
                , fail = \() -> Left chars
                , err = \chars -> Left chars
                }

    run :: [Char] -> Par a -> K4 a b -> Res b
    run chars par k@K4{eps,succ,fail,err} = case par of

      Ret x -> eps x

      Fail -> fail ()

      Satisfy pred -> do
        case chars of
          [] -> fail ()
          c:chars -> if pred c then succ chars c else fail ()

      Alt p1 p2 -> do
        run chars p1 K4{ eps = \a1 ->
                           run chars p2 K4{ eps = \_ -> eps a1 -- left biased
                                          , succ
                                          , fail = \() -> eps a1
                                          , err
                                          }
                       , succ
                       , fail = \() -> run chars p2 k
                       , err
                       }

      Bind par f -> do
        run chars par K4{ eps = \a -> run chars (f a) k
                        , succ = \chars a ->
                            run chars (f a) K4{ eps = \a -> succ chars a -- consume
                                              , succ
                                              , fail = \() -> err chars -- fail->error
                                              , err
                                              }
                        , fail
                        , err
                        }
