module Main where

import Data.Char as C
import Data.List
import Data.Maybe (fromMaybe)
import Maybes (fromJust)


safeHead :: [Char] -> Char
safeHead [] = ' '
safeHead (x : xs) = x

safeLast :: [Char] -> Char
safeLast [] = ' '
safeLast xs = last xs

firstLast :: [Char] -> (Char, Char)
firstLast xs = (safeHead xs, safeLast xs)

filterString :: [Char] -> Int
filterString xs = read $ filter (C.isDigit) xs :: Int

correct :: String -> String
correct xs = map correct' xs

correct' :: Char -> Char
correct' x
  | x == '5' = 'S'
  | x == '0' = 'O'
  | x == '1' = 'I'
  | otherwise = x
  
-- feast :: String -> String -> Bool
-- feast xs ys = s == t
--   where
--     s = firstLast 
--     t = firstLast

noWhiteSpace = map (dropWhile  C.isSpace)

search :: Int -> [Int] -> String
search budget prices = intercalate "," $ map (show ) $ sort $ filter (<= budget) prices
-- search budget prices = intersperse ',' .  $ map ( dropWhile (C.isSpace )) $ sort $ filter (<budget) prices

removeSpace = dropWhile (\x -> x == ' ' )


-- series x
--   | x == 0 = 0
--   | otherwise = 1 + sum [1/(4 + y)  | y <- take t $ multiples 3]
--   where
--     t = round x :: Int

multiples :: Int -> [Int]
multiples n = [x | x <- [0..], x `mod` n == 0]


--  1^n / 3n-2

-- seriesThree = [1^n / (3* n-2 ) | n <- [0..]]


seqEq :: [Double]
seqEq = map (1/) [1,4 ..]

isSortedAndHow :: [Integer] -> String
isSortedAndHow lst 
  | sort lst == lst = "yes, ascending"
  | reverse  (sort lst) == lst = "yes, descending"
  | otherwise = "no"

removeSmallest :: [Int] -> [Int]
removeSmallest xs = error "todo: removeSmallest"

strEnding :: String -> String -> String 
strEnding str end = reverse $ take (length end) . reverse $ str

solution :: String -> String -> Bool 
solution str end = strEnding str end == end 



main :: IO ()
main = do
  let a ="abc"
  let b = "bc"
  print $ strEnding "abc" "bc"
  print $ solution "abc" "d"
