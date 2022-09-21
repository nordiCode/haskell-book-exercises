module Main where

import Data.Char (ord,chr)


shiftNums :: [Char] -> Int -> [Int]
shiftNums xs y = map (\x -> ord x + y) xs

caesar :: [Int] -> String
caesar xs = map (\x -> chr x) xs

main :: IO ()
main = do
    putStrLn "Enter String:"
    xs <- getLine
    putStrLn "How much to rotate:"
    spots <- getLine
    let spot_int = read spots :: Int
    putStrLn $  caesar $ shiftNums xs spot_int