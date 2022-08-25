module Main (main) where
import Data.Char


-- Ex 7 Minimum Length
checkPasswordLength :: String -> Either String String
checkPasswordLength password = 
  case (length password > 20) of 
    True -> Left "Your password is too long"
    False -> Right password

-- Chapter 5 Exercises 
-- Changing to Either from Maybe for error messages
requireAlphaNum :: String -> Either String String
requireAlphaNum xs = case all isAlphaNum xs of
  False -> Left "Not all alphanumeric"
  True -> Right xs

-- only leading whitespace??
cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Space"
cleanWhitespace (x:xs) =
  case isSpace x of
    True -> cleanWhitespace xs 
    False -> Right (x : xs)
    
-- EX.8 Combine them like previous chapter
-- ultimatePassword :: String -> Maybe String
-- ultimatePassword xs = case cleanWhitespace xs of
--   Nothing -> Nothing
--   Just ys -> case requireAlphaNum ys of
--     Nothing -> Nothing
--     Just zs -> case checkPasswordLength zs of
--       Nothing -> Nothing
--       Just ts -> Just ts
  
validatePassword :: String -> Either String String
validatePassword password =
  cleanWhitespace password 
    >>= requireAlphaNum
    >>= checkPasswordLength

-- can run with stack repl
main :: IO ()
main = do
  putStrLn "Please enter a password"
  password <- getLine
  print $ validatePassword password
