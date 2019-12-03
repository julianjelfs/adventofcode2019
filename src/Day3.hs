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
partOne = solve <$> input
 where
  solve = S.lookupMin . S.map manhattanDistance . intersection . fmap
    (S.fromList . path (0, 0) [])

partTwo :: IO (Maybe Int)
partTwo = do
  v <- input
  let paths@[one, two] = path (0, 0) [] <$> v
      ints             = intersection $ S.fromList <$> paths
      c                = combinedSteps one two ints
  pure $ S.lookupMin c

intersection :: [Set (Int, Int)] -> Set (Int, Int)
intersection [a, b] = S.intersection a b
intersection _      = S.empty

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

combinedSteps :: [(Int, Int)] -> [(Int, Int)] -> Set (Int, Int) -> Set Int
combinedSteps one two = S.map (\c -> stepsTo 1 c one + stepsTo 1 c two)

stepsTo :: Int -> (Int, Int) -> [(Int, Int)] -> Int
stepsTo s _ [] = s
stepsTo s c (h : t) | h == c    = s
                    | otherwise = stepsTo (s + 1) c t

toInstructions :: String -> [Instr]
toInstructions = fmap parse . splitOn ","
  where parse i = Instr (head i) (read (tail i))

path :: (Int, Int) -> [(Int, Int)] -> [Instr] -> [(Int, Int)]
path _      sofar []              = sofar
path (x, y) sofar (Instr c d : r) = case c of
  'R' -> path (x + d, y) (append $ (, y) <$> [(x + 1) .. (x + d)]) r
  'U' -> path (x, y + d) (append $ (x, ) <$> [(y + 1) .. (y + d)]) r
  'D' -> path (x, y - d) (append $ (x, ) <$> reverse [(y - d) .. (y - 1)]) r
  'L' -> path (x - d, y) (append $ (, y) <$> reverse [(x - d) .. (x - 1)]) r
  _   -> path (x, y) sofar r
  where append c = sofar <> c
