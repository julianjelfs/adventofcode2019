module Day3 where

import           Data.List.Split                ( splitOn )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

data Instr = Instr Char Int deriving (Show)

input :: IO [[Instr]]
input = do
  l <- lines <$> readFile "data/day3.txt"
  pure $ toInstructions <$> l

partOne :: IO (Maybe Int)
partOne = solve =<< input
 where
  solve = pure . S.lookupMin . S.map manhattanDistance . intersection . fmap
    (path (0, 0) S.empty)

intersection :: [Set (Int, Int)] -> Set (Int, Int)
intersection [a, b] = S.intersection a b
intersection _      = S.empty

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

partTwo :: IO Int
partTwo = do
  v <- input
  pure undefined

toInstructions :: String -> [Instr]
toInstructions = fmap parse . splitOn ","
  where parse i = Instr (head i) (read (tail i))

path :: (Int, Int) -> Set (Int, Int) -> [Instr] -> Set (Int, Int)
path _      sofar []              = sofar
path (x, y) sofar (Instr c d : r) = case c of
  'R' -> path (x + d, y) (merge $ (, y) <$> [(x + 1) .. (x + d)]) r
  'U' -> path (x, y + d) (merge $ (x, ) <$> [(y + 1) .. (y + d)]) r
  'D' -> path (x, y - d) (merge $ (x, ) <$> [(y - d) .. (y - 1)]) r
  'L' -> path (x - d, y) (merge $ (, y) <$> [(x - d) .. (x - 1)]) r
  _   -> path (x, y) sofar r
  where merge = S.union sofar . S.fromList
