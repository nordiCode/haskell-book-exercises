module Main where

-- common to list alphabetically
import Data.Char (toLower, isAlpha)
import Data.List ( sort )


-- undefined in fs allows it to compile 

isAnagram :: String -> String -> Bool
isAnagram xs ys = (sort $ map (toLower) xs) == (sort $ map (toLower) ys)

checkAnagram :: String -> String -> String
checkAnagram xs ys = 
    case isWord xs of 
        Nothing -> "First word is invalid"
        Just xs -> 
            case isWord ys of
                Nothing -> "Second word is invalid"
                Just ys -> 
                    case isAnagram xs ys of
                        False -> "These words are not anagrams!!"
                        True -> "These words are anagrams :)"

isWord :: String -> Maybe String
isWord word =
    case null word of
        True -> Nothing
        False -> case all isAlpha word of
          False -> Nothing
          True -> Just word
                
-- 2.5 Exercises
-- EX 5

alphabet :: [Char]
alphabet = ['a'..'z']  <> ['A'..'Z']

isPalindrome :: String  -> Bool
isPalindrome xs =  word == reverse word
  where
    word = map (toLower) . filter (`elem` alphabet) $ xs

-- EX 6

substituteChar :: Char -> Char
substituteChar c = 
    case c of 
        'e' -> '3'
        'o' -> '0'
        'B' -> '8'
        'b' -> '8'
        'a' -> '4'
        't' -> '7'
        _ -> c

leetSpeak :: [Char] -> [Char]
leetSpeak = map substituteChar


main :: IO ()
main = do
    putStrLn "Please enter a word."
    xs <- getLine
    print $ leetSpeak xs