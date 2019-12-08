module Day08 where

import Control.Lens
import Text.Parsec
import Text.Parsec.Char (digit)
import Control.Monad (replicateM)
import Control.Arrow ((&&&))
import Data.List.Extra (minimumOn)

width, height :: Int
width = 25
height = 6

solveA :: [[[Int]]] -> Int
solveA = calculate . chooseLayer

countDigit :: Int -> [[Int]] -> Int
countDigit d = lengthOf (each . each . filtered (==d))

chooseLayer :: [[[Int]]] -> [[Int]]
chooseLayer = minimumOn (countDigit 0)

calculate :: [[Int]] -> Int
calculate = uncurry (*) . (countDigit 1 &&& countDigit 2)

solveB :: [[[Int]]] -> String
solveB = showImage . foldr1 combine

combine :: [[Int]] -> [[Int]] -> [[Int]]
combine = zipWith (zipWith f)
  where f 2 b = b
        f a _ = a

showImage :: [[Int]] -> String
showImage = unlines . (each . each %~ f)
  where f 0 = '🔵'
        f 1 = '🔴'
        f 2 = ' '

parseInput :: String -> Maybe [[[Int]]]
parseInput = either (const Nothing) Just . parse p ""
  where p        = layers <* spaces <* eof
        layers   = many1 layer
        layer    = replicateM height row
        row      = replicateM width oneDigit
        oneDigit = read . pure <$> digit

main :: IO ()
main = do
  parsed <- parseInput <$> readFile "input.txt"
  case parsed of
    Just inp -> (print . solveA) inp *> (putStrLn . solveB) inp
    Nothing  -> print "parse error"
