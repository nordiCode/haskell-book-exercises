module Main where

import Data.Char as C

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

feast :: String -> String -> Bool
feast xs ys = s == t
  where
    s = firstLast xs
    t = firstLast ys

main :: IO ()
main = do
  print $ correct "ERNE5T HEM1NGWAY - A FARWELL T0 ARM5"
