module Main where


-- add messages to Nothing
-- instead of just returning Nothing 
-- Nothing -> "Password was empty"

checkPasswordLength :: String -> Either String String
checkPasswordLength password = 
  case (length password > 20) of 
    True -> Left "Your password is too long"
    False -> Right password
-- more in chapter_3_4

main :: IO ()
main = do
    putStrLn "yes"