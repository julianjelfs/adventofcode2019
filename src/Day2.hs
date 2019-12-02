module Day2
  ( partOne
  , partTwo
  , toVector
  , processInstructions
  , updateVector
  )
where

import           Data.List.Split                ( splitOn )
import           Data.Vector                   as V

input :: IO (V.Vector Int)
input = toVector <$> readFile "data/day2.txt"

combos :: [(Int, Int)]
combos = [ (n, v) | n <- [0 .. 99], v <- [0 .. 99] ]

partOne :: IO Int
partOne = do
  v <- input
  pure $ processInstructions (v // [(1, 12), (2, 2)])

partTwo :: IO Int
partTwo = do
  v <- input
  pure $ go v combos
 where
  go _ [] = error "ran out of combinations"
  go v ((noun, verb) : c) =
    case processInstructions (v // [(1, noun), (2, verb)]) of
      19690720 -> 100 * noun + verb
      _        -> go v c

processInstructions :: V.Vector Int -> Int
processInstructions v = go v 0
 where
  go v' i = case v' ! i of
    1 -> go (updateVector v' i (+)) (i + 4)
    2 -> go (updateVector v' i (*)) (i + 4)
    _ -> v' ! 0

toVector :: String -> V.Vector Int
toVector = V.fromList . fmap read . splitOn ","

updateVector :: V.Vector Int -> Int -> (Int -> Int -> Int) -> V.Vector Int
updateVector v i binop = v // [(t, binop l r)]
 where
  l = v ! (v ! (i + 1))
  r = v ! (v ! (i + 2))
  t = v ! (i + 3)
