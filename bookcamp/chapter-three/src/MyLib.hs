module MyLib where

import Data.Char
import System.Environment

type NumberedLine = [(Maybe Int, String)]
type NumberedLines = [NumberedLine]

interactiveLines :: Int -> IO ()
interactiveLines counter = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn (show counter ++ ". " ++ line)
      interactiveLines (counter + 1)

lowerLines :: IO ()
lowerLines = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ map toLower line
      lowerLines

parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath
parseArguments _ = Nothing

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")



fileIn :: IO ()
fileIn = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe
    (printHelpText "Missing filename")
    (\filePath -> putStrLn filePath)
    mFilePath

readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath
  return (lines contents)