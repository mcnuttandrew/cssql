module Main
(
    main
)
where

import System.IO
import Data.List
import Lib
import System.Environment


usage :: String
usage = "usage: <proc>\n" ++
        "      convert <CSSQL in file path> <CSS out file path>\n"++
        "      -- Convert a target cssql file into css\n"

transpileFile :: String -> String -> IO()
transpileFile inFile outFile = do
  inputFile <- readFile inFile
  let outputFile = transpile inputFile
  writtenFile <- writeFile outFile outputFile
  putStrLn (inFile ++ " converted")
  return()

main :: IO()
main = do
    args <- getArgs
    case args of
        ["convert", inFile, outFile] -> transpileFile inFile outFile
        _ -> putStrLn usage
