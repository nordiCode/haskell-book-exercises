module Main (main) where

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case length password > 20 of
    True -> Nothing
    False -> Just password

main :: IO ()
main = do
  putStrLn "Please enter a password"
  password <- getLine
  print $ checkPasswordLength password
