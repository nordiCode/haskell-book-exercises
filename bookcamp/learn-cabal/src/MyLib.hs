module MyLib where

import Data.Char ( isPrint )


kilo :: Int
kilo = 1000

type Alphabet = [Char]         


lower :: Alphabet
lower = ['a'..'z']

upper :: Alphabet
upper = ['A'..'Z']

digit :: Alphabet
digit = ['0'..'9']

isUpper :: Char -> Bool
isUpper x = x `elem` upper

isLower :: Char -> Bool
isLower x = x `elem` lower

isDigit :: Char -> Bool
isDigit x = x `elem` digit

isMisc :: Char -> Bool
isMisc x = not (isUpper x || isLower x || isDigit x)

upperRot :: Int -> Char -> Char
upperRot n ch = upper !! ((indexOf ch upper + n) `mod` 26)

lowerRot :: Int -> Char -> Char
lowerRot n ch = lower !! ((indexOf ch lower + n) `mod` 26)

digitRot :: Int -> Char -> Char
digitRot n x = digit !! ((indexOf x digit + n) `mod` 10)

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch 
    | alphabet == lower = lowerRot n ch
    | alphabet == upper = upperRot n ch


indexOf :: Char -> Alphabet-> Int
indexOf ch [] = undefined
indexOf ch (x:xs) = if x == ch then 0 else 1 + indexOf ch xs  

countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters = 
  length . filter (not . isPrint)

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | otherwise = ch

caesar :: Int -> Alphabet -> Alphabet
caesar n [] = []
caesar n message = map (\ch -> rotChar n ch) message

-- higher order function
transformation :: (t -> a) -> [t] -> [a]
transformation fun [] = []
transformation fun (x:xs) = fun x : transformation fun xs

rot135 :: [Char] -> [Char]
rot135 message = map (\ch -> rotSym ch) message

someFunc :: IO ()
someFunc = putStrLn "This is in another file"

-- Book exercise
rotSym :: Char -> Char
rotSym ch
  | isLower ch = lowerRot 13 ch
  | isUpper ch = upperRot 13 ch
  | isDigit ch = digitRot 5 ch
  | otherwise = ch

count' :: Char -> String -> (Char, Int)
count' ch xs =  (ch , length $  (filter (==ch)) xs)


counts :: [Char] -> [(Char,Int)]
counts [] = []
counts (x:xs) = count' x (x:xs) : counts xs