module Day1
  ( solve
  )
where

import           Text.Read                      ( readMaybe )

solve :: IO (Maybe Int)
solve = do
  l <- lines <$> readFile "data/day1.txt"
  pure $ fmap sum . sequence $ fmap fuel . readMaybe <$> l

fuel :: Int -> Int
fuel n | f > 0     = f + fuel f
       | otherwise = 0
  where f = (n `div` 3) - 2

