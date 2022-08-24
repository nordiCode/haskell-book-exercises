{-# LANGUAGE TypeApplications  #-}
module Main where

import Data.Char (toUpper)

f :: String -> String
f x = (drop 2 . map toUpper . reverse) x

-- Monad
-- :type (>>=) 
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- look at chapter_3_4 for monad of password

-- :set -XTypeApplications
-- only one instance of a typeclass

-- EX.10
reverseLine :: IO ()
reverseLine = do
    xs <- getLine
    print $ reverse xs

-- EX.11 
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing func = Nothing
bindMaybe (Just x) func = func x

bindMaybeTwo :: Maybe t -> (t -> Maybe a) -> Maybe a
bindMaybeTwo m func = 
    case m of 
        Nothing -> Nothing
        Just x -> func x

-- EX 12

data StringOrValue a = String String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue m func = case m of
  String s -> String s
  Val a -> func a

main :: IO ()
main = do
  print $ f "hello all"
