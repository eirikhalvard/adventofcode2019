module Day03 where

import Control.Lens
import Text.Parsec
import Text.Parsec.Char (newline, string, digit)
import Control.Applicative
import Control.Monad
import qualified Data.Set as S

data Direction = L | R | U | D deriving (Show)
type WirePart = (Direction, Int)
type Wire = [WirePart]

solveA :: (Wire, Wire) -> Int
solveA (a,b) = minimum
             $ S.map manhattan 
             $ S.delete (0,0)
             $ S.intersection 
                 (walk (0,0) S.empty a)
                 (walk (0,0) S.empty b)
  where
    updateCoords (x,y) L = (x-1,y)
    updateCoords (x,y) R = (x+1,y)
    updateCoords (x,y) U = (x,y+1)
    updateCoords (x,y) D = (x,y-1)
    walk coords s []            = S.insert coords s
    walk coords s ((dir, 0):ws) = walk coords s ws
    walk coords s ((dir, n):ws) = 
      walk (updateCoords coords dir) (S.insert coords s) ((dir, n-1):ws)
    manhattan = uncurry (+) . (each %~ abs)
    

-- solveB :: _ -> _
solveB = undefined

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
  print $ solveA $ parseInput "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83\n"
  print $ solveA wires
  -- print $ solveB wires
  return ()
