module Main where

-- Chapter 1 case expressions

f :: Int -> Int -> Int
f x y = if (x > y) then (x + 10) else y

f2 :: Integer -> Integer -> Integer
f2 x y =
  case x > y of
    False -> y
    True -> x + 10

-- function :: (Ord p, Num p) => p -> p -> p
-- name :: constraints on type variables => type
-- Ord & Num p are typeclasses

-- :info Bool
-- data Bool = False | True

-- :info Maybe
-- data Maybe a = Nothing | Just a

-- datatypes with more than one constructor are sum types

-- Exercises
-- EX.1

absVal :: (Num a, Ord a) => a -> a
absVal x =
  case x > 0 of
    False -> (-1) * x
    True -> x

-- EX.2

validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
  case (null username, null password) of
    (False, False) -> "Okay"
    (False, True) -> "Empty Password"
    (True, False) -> "Empty Username"
    (True, True) -> "Empty username and password"

-- EX.3

-- Doesn't compile because [] is not of type a
-- safeHead :: [a] -> a
-- safeHead [] = []
-- safeHead (x:xs) = x

-- EX.4

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

main :: IO ()
main = do
  print $ f 10 20
  print $ absVal (-10)
  print $ validateUsernamePassword "hello" ""
  print $ safeHead [1, 2, 3]
