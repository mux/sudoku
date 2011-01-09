module Sudoku (solve) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

-- A sudoku solver I wrote for Project Euler problem #96.
--
-- http://projecteuler.net/index.php?section=problems&id=96

type Grid = [[Int]]
type Node = (Int,Int)

gridNodes :: Grid -> [(Node,Int)]
gridNodes = concat . walk
  where walk g = zipWith (\r i -> zipWith (\x j -> ((i,j), x)) r [1..]) g [1..]

neighbors :: Node -> [Node]
neighbors (i,j) = [ (i,k) | k <- [1..9], k /= j ] ++
                  [ (k,j) | k <- [1..9], k /= i ] ++
                  [ (m,n) | m <- boxRange i, n <- boxRange j, m /= i && n /= j ]
  where boxRange x = let k = (x - 1) `div` 3 in [k * 3 + 1 .. (k + 1) * 3]

solve :: Grid -> [Map Node Int]
solve g = let (empty,filled) = partition ((==0) . snd) (gridNodes g)
          in solve' (map fst empty) (M.fromList filled)
  where solve' (n:ns) colors = choose colors n >>= \c ->
                                 solve' ns (M.insert n c colors)
        solve' []     colors = return colors

        choose colors n = [1..9] \\ mapMaybe (`M.lookup` colors)
                                                    (neighbors n)
