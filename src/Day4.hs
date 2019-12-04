module Day4 where

import           Data.Char                      ( digitToInt )

puzzleRange :: [Int]
puzzleRange = [284639 .. 748759]

partOne :: Int
partOne = length (filter meetsCriteria puzzleRange)

meetsCriteria :: Int -> Bool
meetsCriteria n = containsPair n && not (decreasing n)

containsPair :: Int -> Bool
containsPair n = dup Nothing str
 where
  dup _       []      = False
  dup Nothing (c : r) = dup (Just c) r
  dup (Just p) (c : r) | c == p    = True
                       | otherwise = dup (Just c) r
  str = show n

decreasing :: Int -> Bool
decreasing n = go Nothing str
 where
  go _       []      = False
  go Nothing (c : r) = go (Just c) r
  go (Just p) (c : r) | digitToInt p > digitToInt c = True
                      | otherwise                   = go (Just c) r
  str = show n
