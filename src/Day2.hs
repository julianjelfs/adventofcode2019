module Day2
  ( solve
  , toVector
  , processInstructions
  , updateVector
  )
where

import           Data.List.Split                ( splitOn )
import           Data.Vector                   as V

solve :: IO Int
solve = do
  v <- toVector <$> readFile "data/day2.txt"
  pure $ processInstructions (v // [(1, 12), (2, 2)])

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
