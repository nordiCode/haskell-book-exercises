{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char as C hiding (isLower,isUpper)
import Data.List
import Data.Maybe (fromMaybe)
import Maybes (fromJust)

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch =
  alphabet !! ((indexOf 0 ch alphabet + n) `mod` length alphabet)

upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | otherwise = ch

caesar :: Int -> String -> String
caesar n message = map (\ch -> rotChar n ch) message

rot13 :: String -> String
rot13 message = caesar 13 message

vowels :: Alphabet
vowels = "aeiouAEIOU"

alphabet :: Alphabet
alphabet = lowerAlphabet ++ upperAlphabet

consonants :: [Char]
consonants = filter (`notElem` vowels) alphabet

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

-- feast :: String -> String -> Bool
-- feast xs ys = s == t
--   where
--     s = firstLast 
--     t = firstLast

noWhiteSpace :: [[Char]] -> [[Char]]
noWhiteSpace = map (dropWhile  C.isSpace)

search :: Int -> [Int] -> String
search budget prices = intercalate "," $ map (show ) $ sort $ filter (<= budget) prices
-- search budget prices = intersperse ',' .  $ map ( dropWhile (C.isSpace )) $ sort $ filter (<budget) prices

removeSpace :: [Char] -> [Char]
removeSpace = dropWhile (\x -> x == ' ' )


-- series x
--   | x == 0 = 0
--   | otherwise = 1 + sum [1/(4 + y)  | y <- take t $ multiples 3]
--   where
--     t = round x :: Int

multiples :: Int -> [Int]
multiples n = [x | x <- [1..], x `mod` n == 0]


--  1^n / 3n-2

-- seriesThree = [1^n / (3* n-2 ) | n <- [0..]]


seqEq :: [Double]
seqEq = map (1/) [1,4 ..]

isSortedAndHow :: [Integer] -> String
isSortedAndHow lst
  | sort lst == lst = "yes, ascending"
  | reverse  (sort lst) == lst = "yes, descending"
  | otherwise = "no"

removeSmallest :: [Int] -> [Int]
removeSmallest xs = error "todo: removeSmallest"

strEnding :: String -> String -> String
strEnding str end = reverse $ take (length end) . reverse $ str

endsWith :: String -> String -> Bool
endsWith str end = strEnding str end == end

duplicateCount :: String -> Int
duplicateCount = undefined

summation :: Integer -> Integer
summation n = sum [1..n]

smallEnough :: [Int] -> Int -> Bool
smallEnough xs v = all (<=v) xs

multiplesToo :: Int -> Int -> [Int]
multiplesToo x y = takeWhile (<=y) $ multiples x

consonantCount :: String -> Int
consonantCount = length . filter ( `elem` consonants)

indexOf :: Int -> Char -> Alphabet -> Int
indexOf n ch [] = undefined
indexOf n ch (x : xs) = if x == ch then n else 1 + indexOf n ch xs


indexInt :: Int -> [Int] -> Int -> Int
indexInt n nums count
  | count == length nums = (-1)
  | n == (nums !! count) = count
  | otherwise = indexInt n nums (count + 1)

sumWord :: [Char] -> Int
sumWord  [] = 0
sumWord  (x : xs) = indexOf 1 x lowerAlphabet + sumWord xs

high :: String -> String
high myStr = (words myStr) !! (indexInt (maximum vals) vals 0)
  where
    vals = map sumWord $ words myStr

evenTwins :: [Int] -> Int
evenTwins xs = length $ nub [ (x,y) | x <- xs, y <- xs, x<y, (x+y) `mod` 2 == 0]

listAlt n s1 s2 = if n > 0 && n `mod` 2 == 0 then s2 : listAlt (n-1) s1 s2 else s1 : listAlt (n-1) s1 s2

sumAll n = if n > 0 then n + sumAll (n-1) else 0


readInt :: String -> Int
readInt = read

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = filter (not . p) xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = filter p xs

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x:xs) (y:ys)
  | x == y = distance xs ys
  | otherwise = (+1) <$> distance xs ys



decode :: String -> String
decode [] = []
decode (x:[]) = [x]
decode (x:xs)  
  | C.isDigit x = go (digitToInt x) xs 
  | otherwise = x : decode xs 
   where
    go x (y:ys)  
      | C.isDigit y = go (readIntTwo  (show x ++ [y])) ys 
      | otherwise = replicate x y ++ decode ys 


test x = if C.isDigit x then "yo" else "noooo"

readIntTwo :: String -> Int
readIntTwo = read

encode :: String -> String
encode text = intercalate "" . map toRleString $ zip x y
  where
    y = groupBy (==) text
    x = map length y
    toRleString (a,b)
      | a == 1 = b
      | b == " " = "b "
      | otherwise = show a ++  [head b]


main :: IO ()
main = do
  putStrLn  "you"
  
  -- putStrLn $ show $ areaTri 1 1
  -- print $ words s
  -- let vals =  map sumWord $ words s
  -- print $ maximum vals
  -- print $  vals
  -- print $ indexInt 5 [1,2,3,4] 0
  -- print $ (words s) !! (indexInt (maximum vals) vals 0)