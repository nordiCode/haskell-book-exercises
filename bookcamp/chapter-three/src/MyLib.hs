module MyLib where

import Data.Maybe
import Data.Char
import System.Environment

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]

interactiveLines :: Int -> IO ()
interactiveLines counter = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn (show counter ++ ". " ++ line)
      interactiveLines (counter + 1)

upperLines :: Int -> IO ()
upperLines counter = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn (show counter <> ". " <> map toUpper line)
      upperLines (counter + 1)

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


numberAllLines :: [String] -> NumberedLines
numberAllLines lines =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x : xs) = (Just counter, x) : go (counter + 1) xs
   in go 1 lines

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber lines =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x : xs) =
        let mNumbering = if shouldNumber x then Just counter else Nothing
            newCounter = if shouldIncr x then counter + 1 else counter
         in (mNumbering, x) : go newCounter xs
  in go 1 lines

isEmpty :: String -> Bool 
isEmpty str = 
  null str || all (\s -> not (isPrint s) || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty str = not (isEmpty str)