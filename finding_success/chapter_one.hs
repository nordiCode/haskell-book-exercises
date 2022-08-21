module Main where

-- case expressions

f :: Int -> Int -> Int
f x y = if (x > y) then (x + 10) else y

main :: IO ()
main = do
    print $ f 10 20