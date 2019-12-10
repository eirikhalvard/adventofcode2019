module Day10 where

import Control.Lens
import Data.List.Extra (nubOn)
import Data.Tuple (swap)

type Asteroid = (Int,Int)

parseInput :: String -> [Asteroid]
parseInput = toListOf ((itraversed<.>itraversed)
                      . filtered (=='#')
                      . withIndex
                      . to (swap . fst))
           . lines

solveA :: [Asteroid] -> Int
solveA positions = maximum (fmap numInSight positions)
  where
    numInSight pos = length $ nubOn (getAngleFrom pos) positions
    getAngleFrom a b = uncurry atan2 . (each %~ fromIntegral) $ coordDiff a b
    coordDiff (y,x) (y',x') = (y-y', x-x')
                          


main :: IO ()
main = do
  xs <- readFile "input.txt"
  print . solveA . parseInput $ xs
