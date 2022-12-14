module Main where

import MyLib 


main :: IO ()
main = do
  print $ counts "hello"
  print $ counts $ caesar 2 "hello"
