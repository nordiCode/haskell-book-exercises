
solve :: [Char] -> Int
solve  = length . filter (`elem` vowels) 

vowels :: [Char]
vowels = "aeiouAEIOU"

readInput :: String -> [Int]
readInput = (map read ) . words

readInt :: String -> Int
readInt = read

-- main :: (String -> String) -> IO ()
-- main = interact 

main :: IO ()
main = do
  s <- getContents
  print $ words s

