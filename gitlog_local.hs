module Main where

import Data.List
import System.Process

-- message to make commit
main :: IO ()
main = do
  output <- readProcess "git" ["log", "--format=%ad %s"] ""
  let entries = map parseEntry $ lines output
      grouped = groupBy (\x y -> fst x == fst y) entries
  mapM_ print grouped

parseEntry :: String -> (String, String)
parseEntry line = case words line of
  (date:message) -> (date, unwords message)
  _ -> error $ "Invalid line: " ++ line