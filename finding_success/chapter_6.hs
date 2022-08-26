module Main where

-- type String = [Char]
-- is known as a type alias 
-- another name for the same thing

-- type Password = String
-- type Username = String

newtype Password = Password String 
  deriving Show 

newtype Error = Error String
  deriving Show

newtype Username = Username String 
  deriving Show

x :: Password
x = "julielovesbooks"

userOne :: Username
userOne = "tina"

greet :: Username -> IO ()
greet username = print ("hello, " ++ username)

main :: IO ()
main = do
    greet userOne