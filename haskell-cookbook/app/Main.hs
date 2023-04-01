module Main where

import MyLib
import System.Environment
import MyLib (printHelpText)

main :: IO ()
main = do 
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe
    (printHelpText "Missing filename")
    (\filePath -> putStrLn filePath)
    mFilePath
