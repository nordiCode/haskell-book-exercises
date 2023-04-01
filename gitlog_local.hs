-- {-# OPTIONS_GHC -package old-locale #-}
module Main where

import Data.List
import System.Process
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar



parseEntry :: String -> (String, String)
parseEntry line = case words line of
  (date:message) -> (date, unwords message)
  _ -> error $ "Invalid line: " ++ line

formatGroupedEntries :: [[(String, String)]] -> String
formatGroupedEntries grouped = unlines $ map formatGroup grouped

formatGroup :: [(String, String)] -> String
formatGroup entries@(x:xs) =
  "## " ++ fst x ++ "\n\n" ++ "- " ++ unwords (map snd entries) ++ "\n"
formatGroup [] = error "Empty group"

reportISODate :: IO String
reportISODate = do
  currentTime <- getCurrentTime
  let sevenDaysAgo = addDays (-7) (utctDay currentTime)
      (year, month, day) = toGregorian sevenDaysAgo
      isoDateString = formatTime defaultTimeLocale "%Y-%m-%d" sevenDaysAgo
  return isoDateString
 

main :: IO ()
main = do
  let filename = "test_output.md"
    -- , "--since='1 week ago'"  add for just the one week of commits
  output <- readProcess "git" ["log", "--format=%ad %s", "--date=short"] ""
  let entries = map parseEntry $ lines output
      grouped = groupBy (\x y -> fst x == fst y) entries
      formatted = formatGroupedEntries grouped

  print formatted
  let test_output =  formatted
  reportDate <- reportISODate
--   let test_output = concat grouped
  writeFile filename ("## Grant's Report for Week of "<>reportDate <> "\n" <> test_output)

