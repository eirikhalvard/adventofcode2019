module Day04 where

import Control.Lens
import Text.Parsec
import Text.Parsec.Char (newline, string, digit)
import Control.Applicative
import Control.Monad
import Data.List

increasingSixes :: [Int]
increasingSixes = do
  a <- [1..9]
  b <- [a..9]
  c <- [b..9]
  d <- [c..9]
  e <- [d..9]
  f <- [e..9]
  return . read . concatMap show $ [a,b,c,d,e,f]

solve :: (Int -> Bool) -> (Int,Int) -> Int
solve comp (l,r) = length
                 . takeWhile (<r)
                 . filter (any comp . fmap length . group . show)
                 . dropWhile (<l) 
                 $ increasingSixes

parseInput :: String -> Maybe (Int,Int)
parseInput = either (const Nothing) Just . parse p ""
  where p = (,) <$> (i <* string "-") <*> i
        i = read <$> many1 digit


main :: IO ()
main = case parseInput "356261-846303" of
         Just inp -> mapM_ (print . ($ inp) . solve) [(>=2), (==2)]
         Nothing  -> print "parse error"
