module Main where

-- set builder notation

setPairs :: [(Int, Int)]
setPairs = [(x, y) | x <- [1, 2, 3], y <- [1, 2, 3], x /= y]

-- monadic do notation of the set

setDo :: [(Integer, Integer)]
setDo = do
  x <- [1, 2, 3]
  y <- [1, 2, 3]
  True <- return (x /= y)
  return (x, y)

-- True is a set requirment

setDouble :: [(Integer, Integer)]
setDouble = do
  x <- [1 .. 4]
  y <- [1 .. 4]
  True <- return (x <= y)
  return (2 * x, 2 * y)

getPassword :: IO String
getPassword = do
  xs <- getLine
  return xs

doubleInt :: IO ()
doubleInt = do
  putStrLn "Enter a number"
  s <- getLine
  let num = (read s :: Int) * 2
  putStrLn $ "Your number doubled = " <> show num

doubleList :: IO ()
doubleList = do
  putStrLn "Enter first number"
  s <- getLine
  putStrLn "Enter number greater than first"
  s2 <- getLine
  let numList = [minS .. maxS] where
      minS = (read s :: Int)
      maxS = (read s2 :: Int)
  putStrLn $ "Your list " <> show numList

lookForThree :: [Integer]
lookForThree = do
    a <- [1,2,3]
    b <- [a + 1]
    if b == 3 then [] else [b]

main :: IO ()
main = do
  print lookForThree
