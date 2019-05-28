import System.IO
import Data.List
import Lib
import System.Environment

checkFileEquality :: String -> String -> IO Bool
checkFileEquality leftFilePath rightFilePath = do
  leftFile <- readFile leftFilePath
  rightFile <- readFile rightFilePath
  return (leftFile == rightFile)

testNames =
  [ "simple"
  ,  "join"
  ,  "merge-and-deleted"
  ,  "copy-rename-drop"]
testSource     name = "./test/tests/" ++ name ++ ".cssql"
testSink       name = "./test/tests/" ++ name ++ ".css"
testExpected   name = "./test/tests/" ++ name ++ "-expected.css"

runtest :: String -> IO Bool
runtest testPrefix = do
  putStrLn ("\nExecuting test: " ++ testPrefix)
  x <- transpileFile (testSource testPrefix) (testSink testPrefix)
  testResult <- checkFileEquality (testExpected testPrefix) (testSink testPrefix)
  putStrLn (testPrefix ++ " test " ++ (if testResult then "PASSED" else "FAILED"))
  return testResult

transpileFile :: String -> String -> IO()
transpileFile inFile outFile = do
  inputFile <- readFile inFile
  let outputFile = transpile inputFile
  writtenFile <- writeFile outFile outputFile
  putStrLn (inFile ++ " converted")
  return()

main :: IO()
main = do
  putStrLn "RUNNING TEST SUITE"
  -- passCount <- foldr (\ testPrefix acc -> if runtest testPrefix then acc + 1 else acc) runtest testNames
  passes <- mapM runtest testNames
  let passCount = length (filter id passes)
  putStrLn ("\nPASSED " ++ show passCount ++ " out of " ++ show (length testNames) ++ " tests." )
