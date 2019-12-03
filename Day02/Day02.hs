module Day02 where

import Control.Lens
import Text.Parsec
import Text.Parsec.Char (string, digit)
import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Vector as V

solveA :: [Int] -> Maybe Int
solveA = compute 0 . startVec 12 2

solveB :: [Int] -> [(Int, Int)]
solveB xs = do
  noun <- [0..99]
  verb <- [0..99]
  let vec = startVec noun verb xs
  guard $ compute 0 vec == Just 19690720
  return (noun,verb)

startVec :: Int -> Int -> [Int] -> V.Vector Int
startVec noun verb = (ix 2 .~ verb) . (ix 1 .~ noun) . V.fromList

compute :: Int -> V.Vector Int -> Maybe Int
compute pos vec = do
  [op, a1, a2, res] <- traverse (\i -> vec ^? ix (pos + i)) [0..3]
  let updateVec f = (\val -> set (ix res) val vec) 
                 <$> liftA2 f (vec ^? ix a1) (vec ^? ix a2)
  case op of
    1  -> compute (pos+4) <=< updateVec $ (+)
    2  -> compute (pos+4) <=< updateVec $ (*)
    99 -> preview (ix 0) vec
    x  -> Nothing

parseInput :: String -> [Int]
parseInput xs = 
  case parse (p <* spaces <* eof) "" xs of
    Left err -> error (show err)
    Right res -> res
  where
    p = fmap read <$> sepBy (many1 digit) (string ",")

main :: IO ()
main = do
  xs <- parseInput <$> readFile "input.txt"
  print $ solveA xs
  print $ solveB xs
  return ()
