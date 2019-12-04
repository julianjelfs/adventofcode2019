module Day4 where

import           Data.Char                      ( digitToInt )
import           Data.List                      ( group )

solve :: Int
solve = length (filter meetsCriteria [284639 .. 748759])

meetsCriteria :: Int -> Bool
meetsCriteria n = containsPair n && not (decreasing n)

containsPair :: Int -> Bool
containsPair n = any pair grouped
 where
  grouped = group $ show n
  pair g = length g == 2

decreasing :: Int -> Bool
decreasing n = go Nothing str
 where
  go _       []      = False
  go Nothing (c : r) = go (Just c) r
  go (Just p) (c : r) | digitToInt p > digitToInt c = True
                      | otherwise                   = go (Just c) r
  str = show n
