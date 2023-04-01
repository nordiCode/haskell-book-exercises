module Main where
import Data.Char
import Data.List
-- tripletsWithSum :: Int -> [(Int, Int, Int)]
-- tripletsWithSum sumLi = filter (==sumLi) [(a,b,c) | a<-[1..sum],b<-[1..sum],c<-[1..sum] , a< b && b<c, a^2 + b^2 == c^2]

isValid :: String -> Bool
isValid n = checkLength n && luhnSum n `mod` 10 == 0
  where
    luhnSum xs  = if intermedResult xs > 9 then intermedResult xs - 9 else intermedResult xs
    intermedResult xs = sum $ map doubleSeconds $ reverse $ zip [1..] (cleanString xs) 
    checkLength xs = length (cleanString xs) > 0
    cleanString xs = filter (/= ' ') xs


doubleSeconds :: (Int,Char) -> Int
doubleSeconds (a,b)
  | a `mod` 2 == 0 && (2 * digitToInt b) > 9 = 2 * digitToInt b - 9
  | a `mod` 2 == 0 = 2 * digitToInt b
  | otherwise = digitToInt  b


main :: IO ()
main = do
  print $ "yo"