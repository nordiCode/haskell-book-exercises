module Main (main) where
import Data.Char
import InteractiveEval (Term(val))
import Data.Text.Internal.Encoding.Utf32 (validate)

-- Ex 7 Minimum Length
checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password > 20 || length password < 5) of
    True -> Nothing
    False -> Just password


requireAlphaNum :: String -> Maybe String
requireAlphaNum xs = case all isAlphaNum xs of
  False -> Nothing
  True -> Just xs

-- only leading whitespace??
cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x:xs) =
  case isSpace x of
    True -> cleanWhitespace xs 
    False -> Just (x : xs)
    
-- EX.8 Combine them like previous chapter
ultimatePassword :: String -> Maybe String
ultimatePassword xs = case cleanWhitespace xs of
  Nothing -> Nothing
  Just ys -> case requireAlphaNum ys of
    Nothing -> Nothing
    Just zs -> case checkPasswordLength zs of
      Nothing -> Nothing
      Just ts -> Just ts
  
validatePassword :: String -> Maybe String
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
