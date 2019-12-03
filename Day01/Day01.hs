import Control.Arrow
import Control.Lens

main :: IO ()
main = do
  xs <- readFile "input.txt"
  print . solveA $ xs -- 3497399
  print . solveB $ xs -- 5243207

solveA :: String -> Int
solveA = sum . fmap (calc . read) . words
    
solveB :: String -> Int
solveB = sumOf (each.each) . fmap (iter . read) . words
  where
    iter = tail . takeWhile (0 <) . iterate calc
    
calc :: Int -> Int
calc = (`div` 3) >>> subtract 2


