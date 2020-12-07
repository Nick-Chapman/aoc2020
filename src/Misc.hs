
module Misc (check,readInts,bfs,bfs') where

import Data.Set (Set,union)
import qualified Data.Set as Set


check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

readInts :: FilePath -> IO [Int]
readInts path = do
  str <- readFile path
  pure $ map read (words str)


-- with tracing in IO
bfs' :: Ord a => (a -> [a]) -> [a] -> IO (Set a)
bfs' step initial = loop (0::Int) Set.empty (Set.fromList initial)
  where
    loop n acc frontier = do
      putStrLn $ concat ["bfs-step:", show n, ", #acc=", show (Set.size acc), ", #fronier=", show (Set.size frontier) ]
      if Set.null frontier then return acc else do
        let acc' = acc `union` frontier
        let frontier' = Set.fromList [ a2 | a1 <- Set.toList frontier, a2 <- step a1, a2 `notElem` acc' ]
        loop (n+1) acc' frontier'


-- without tracing
bfs :: Ord a => (a -> [a]) -> [a] -> Set a
bfs step initial = loop Set.empty (Set.fromList initial)
  where
    loop acc frontier = do
      if Set.null frontier then acc else do
        let acc' = acc `union` frontier
        let frontier' = Set.fromList [ a2 | a1 <- Set.toList frontier, a2 <- step a1, a2 `notElem` acc' ]
        loop acc' frontier'
