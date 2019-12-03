module Day03 where

import Control.Lens
import Text.Parsec
import Text.Parsec.Char (newline, string, digit)
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Lazy as M

data Direction = L | R | U | D deriving (Show)
type WirePart = (Direction, Int)
type Wire = [WirePart]

updateCoords :: (Int, Int) -> Direction -> (Int, Int)
updateCoords (x,y) L = (x-1,y)
updateCoords (x,y) R = (x+1,y)
updateCoords (x,y) U = (x,y+1)
updateCoords (x,y) D = (x,y-1)

solveA :: (Wire, Wire) -> Int
solveA (a,b) = minimum
             $ S.map manhattan 
             $ S.delete (0,0)
             $ S.intersection 
                 (walk (0,0) S.empty a)
                 (walk (0,0) S.empty b)
  where
    walk coords s []            = S.insert coords s
    walk coords s ((dir, 0):ws) = walk coords s ws
    walk coords s ((dir, n):ws) = 
      walk (updateCoords coords dir) (S.insert coords s) ((dir, n-1):ws)
    manhattan = uncurry (+) . (each %~ abs)
    

solveB :: (Wire, Wire) -> Int
solveB (a,b) = minimum
             $ M.delete (0,0)
             $ M.intersectionWith (+)
                 (walk 0 (0,0) M.empty a)
                 (walk 0 (0,0) M.empty b)
  where
    walk steps coords m []
      | M.member coords m = m
      | otherwise = M.insert coords steps m
    walk steps coords m ((dir, 0):ws) = walk steps coords m ws
    walk steps coords m ((dir, n):ws)
      | M.member coords m = walk (steps+1) (updateCoords coords dir) 
          m ((dir, n-1):ws)
      | otherwise = walk (steps+1) (updateCoords coords dir) 
          (M.insert coords steps m) ((dir, n-1):ws)
    

parseInput :: String -> (Wire, Wire)
parseInput xs = 
  case parse (wires <* spaces <* eof) "" xs of
    Left err -> error (show err)
    Right [a,b] -> (a,b)
    Right xs -> error "too many wires, only support two"
  where
    wires = endBy wire newline
    wire = sepBy wirePart (string ",")
    wirePart = liftA2 (,) direction nat
    direction = toDir <$> oneOf "LRUD"
    nat = read <$> many1 digit
    toDir 'L' = L
    toDir 'R' = R
    toDir 'U' = U
    toDir 'D' = D


main :: IO ()
main = do
  wires <- parseInput <$> readFile "input.txt"
  print $ solveA wires
  print $ solveB wires
