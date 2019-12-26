module Day6 where

import           Convertible                    ( convert )
import           Data.Text                      ( splitOn )

data Tree = Tree Int String [Tree] deriving Show

partOne :: IO Int
partOne = totalOrbits . lines <$> readFile "data/day6.txt"

makeTree :: [String] -> Tree
makeTree inp = go (Tree 0 "COM" []) $ split <$> inp
 where
  go (Tree n p _) lk =
    Tree n p (fmap (\c -> go (Tree (n + 1) c []) lk) (findChildren lk p))
  findChildren lk parent = snd <$> filter (\(p, _) -> p == parent) lk

split :: String -> (String, String)
split s = case splitOn ")" . convert $ s of
  [a, b] -> (convert a, convert b)
  _      -> undefined

totalOrbits :: [String] -> Int
totalOrbits inp = sum $ go [] (makeTree inp)
 where
  go depths (Tree n _ []      ) = n : depths
  go depths (Tree n _ children) = n : concatMap (go depths) children

testInput :: [String]
testInput =
  [ "COM)B"
  , "B)C"
  , "C)D"
  , "D)E"
  , "E)F"
  , "B)G"
  , "G)H"
  , "D)I"
  , "E)J"
  , "J)K"
  , "K)L"
  ]
