{-# LANGUAGE TupleSections #-}
module Day10 where

import Control.Lens
import Data.List.Extra (nubOn, minimumOn, maximumOn, groupOn, sortOn)
import Data.Tuple (swap)

type Asteroid = (Int,Int)

parseInput :: String -> [Asteroid]
parseInput = toListOf ((itraversed<.>itraversed)
                      . filtered (=='#')
                      . withIndex
                      . to (swap . fst))
           . lines

solveA :: [Asteroid] -> (Int, Asteroid)
solveA positions = maximumOn fst (fmap numInSight positions)
  where
    numInSight pos = (,pos) . length $ nubOn (angleFrom pos) positions

solveB :: Asteroid -> [Asteroid] -> Int
solveB pos positions = asteroidX * 100 + asteroidY
  where 
    (asteroidX, asteroidY) = minimumOn (lengthFrom pos) 
                           . (!! 199) 
                           . groupOn f 
                           . sortOn f 
                           . filter (/= pos) 
                           $ positions
    f = angleFromTop pos
    lengthFrom (x,y) (x',y') = (y-y')^2 + (x-x')^2


angleFromTop :: Asteroid -> Asteroid -> Float
angleFromTop (x,y) (x',y') = pi - atan2 (fromIntegral (x'-x)) (fromIntegral (y'-y))

angleFrom :: Asteroid -> Asteroid -> Float
angleFrom a b = uncurry atan2 . (each %~ fromIntegral) $ coordDiff a b
  where coordDiff (y,x) (y',x') = (y'-y, x'-x)


main :: IO ()
main = do
  xs <- readFile "input.txt"
  let asteroids = parseInput xs
      (amount, largest) = solveA asteroids
  putStrLn ("Amount: " ++ show amount ++ ", Largest: " ++ show largest)
  print $ solveB largest asteroids
