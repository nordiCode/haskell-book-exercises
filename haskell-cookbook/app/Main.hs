module Main where

import MyLib
import Control.Monad.State

incrementState :: State Int Int
incrementState = do
  n <- get 
  put (n + 1)
  return n

firstTuple :: (Int,Int) -> Int
firstTuple (x,_) = x

main :: IO ()
main = do
  let x = runState incrementState 1
  print $ firstTuple x
