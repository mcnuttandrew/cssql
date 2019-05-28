module Lib
    ( transpile
    ) where

import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Data.Semigroup
import qualified Data.Foldable as F

-- TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES
-- TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES
-- TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES

data Database = Database {
  tables :: Map.Map String Table,
  maxOrder :: Int
  } -- deriving (Show)
data Table = Table {
  selector :: String,
  attributes :: [Attribute],
  order :: Int
  -- TODO VIEWS, which encompass media queries, selector states etc
  } deriving (Eq)
data Attribute = Attribute {
  cssKey :: String,
  cssVal :: String} deriving (Ord, Eq)

data ParserFunction = ParserFunction {
  fun :: Database -> Database,
  funType :: String,
  args :: [String]
  }

instance Show Attribute where
  show x = "  " ++ cssKey x ++ ": " ++ cssVal x ++ ";"

instance Show Table where
  show x = selector x ++ " {\n" ++ formattedRows ++ "\n}\n"
    where
      sortedRows = sort (attributes x)
      formattedRows = intercalate "\n" (map show sortedRows)

instance Ord Table where
  compare a b
    | order a <= order b = GT
    | order a > order b = LT
    | otherwise = LT

instance Show Database where
  show Database{tables = tabs} = foldr (\ tab acc -> acc ++ show tab) "" orderedTables
    where
      -- tables must be ordered by their input order, the cascade still stands
      orderedTables = sort (map snd (Map.toList tabs))

instance Show ParserFunction where
  show x = funType x ++ "(" ++ intercalate ", " (args x) ++ ")"

instance Semigroup Table where
  a <> b = Table {
    selector = selector a ++ ",\n" ++ selector b,
    attributes = mergedAttributes,
    order = 1 + max (order a) (order b)
    }
    where
      merger = foldr (\ atrib acc -> Map.insert (cssKey atrib) (cssVal atrib) acc)
      mergeLeft = merger Map.empty (attributes a)
      toAttribute (k, v) = Attribute{cssKey = k, cssVal = v}
      mergedAttributes = map toAttribute (Map.toList (merger mergeLeft (attributes b)))

-- "CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS"
-- "CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS"
-- "CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS"

addAttributeToTable :: Attribute -> Table -> Table
addAttributeToTable atrib tb = tb {
    attributes = (++) (filter (\ x -> cssKey x /= cssKey atrib) (attributes tb)) [atrib]
  }

removeAttributeFromTable :: Table -> String -> Table
removeAttributeFromTable tb key = tb{attributes = filter (\ x -> cssKey x /= key) (attributes tb)}

emptyDatabase :: Database
emptyDatabase = Database {tables = Map.empty, maxOrder = 0}

emptyTable :: String -> Table
emptyTable str = Table {selector = str, attributes = [], order = -1}

insertTableIntoDB :: Database -> Table -> Database
insertTableIntoDB db tab = Database {tables = newTables, maxOrder = tabOrder + 1}
  where
    tabOrder = maxOrder db
    selectorName = selector tab
    newTable = tab {order = tabOrder}
    newTables = Map.insert selectorName newTable (tables db)


updateTableInDB :: Database -> Table -> Database
updateTableInDB db tab = db {tables = Map.insert (selector tab) tab (tables db)}

dropTableInDB :: Database -> String -> Database
dropTableInDB db selectorName = db {tables = Map.delete selectorName (tables db)}

getTable :: Database -> String -> Table
getTable db tableName = case Map.lookup tableName (tables db) of
    Just x -> x
    Nothing -> error("Selector \"" ++ tableName ++ "\" not found")

-- MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES
-- MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES
-- MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES

transpile :: String -> String
transpile cssqlInput = show (executeStatements parsedStatements)
  where
    parsedStatements = map parseStatement (cleanIntoStatements cssqlInput)

executeStatements :: [ParserFunction] -> Database
executeStatements = foldl (flip fun) emptyDatabase

cleanIntoStatements :: String -> [String]
cleanIntoStatements txt = filter (not . null) (splitOn ";" (filter (/= '\n') noComments))
    where
      -- remove comments at the parsing stage
      isComment str = (==) (take 2 str) "//"
      -- in the future they could maybe be left in? though there would have to be some extra decoration
      noComments = concat (filter (not . isComment) (splitOn "\n" txt))

toArgs :: String -> [String]
toArgs = splitOn " "


-- PARSING PARSING PARSING PARSING PARSING
-- PARSING PARSING PARSING PARSING PARSING
-- PARSING PARSING PARSING PARSING PARSING


parseStatement :: String -> ParserFunction
parseStatement commandStr =
  -- TODO: this should probably evolve into a regex or parser thing
  case funcName of
     "CREATE" -> createTable commandStr
     "COPY"   -> createCopy commandStr
     "DELETE" -> createDelete commandStr
     "DROP"   -> createDrop commandStr
     "INSERT" -> createInsert commandStr
     "MERGE"  -> createMerge commandStr
     "RENAME" -> createRename commandStr
     _        -> error ("Command not recognized: " ++ funcName)
     where
       funcName = head (toArgs commandStr)

createIdentity :: String -> ParserFunction
createIdentity _ = ParserFunction {fun = boundId, funType = "identity", args = []}
  where
    boundId x = x

-- EXAMPLE SYNTAX: CREATE SELECTOR <SELECTOR>;
createTable :: String -> ParserFunction
createTable commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createTable",
  args = [tableName]
  }
  where
    (tableName:xs) = extractArgs ["CREATE SELECTOR "] commandStr
    boundFunc db = insertTableIntoDB db (emptyTable tableName)

extractInsertArgs :: [[String]] -> (String, String, String)
extractInsertArgs [[fullStr, target, key, val]] = (target, key, val)
extractInsertArgs _ = error "Syntax error in insert"

validateInsertArgs :: [String] -> (String, String, String)
validateInsertArgs [tableName, key, val] = (tableName, key, val)
validateInsertArgs x = error ("INSERT REQUIRES AT EXACTLY THREE ARGUMENTS.\nProblem line: " ++ show x)

createInsert :: String -> ParserFunction
createInsert commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createInsert",
  args = [tableName, key, val]
  }
  where
    (tableName, key, val) = validateInsertArgs (extractArgs ["INSERT ", " (", ", ", ")"] commandStr)
    addAtrib = addAttributeToTable (Attribute{cssKey = key, cssVal = val})
    boundFunc db = updateTableInDB db (addAtrib (getTable db tableName))

createDelete :: String -> ParserFunction
createDelete commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createDelete",
  args = [tableName, attributeName]
  }
  where
  (funName:tableName:attributeName:xs) = toArgs commandStr

  boundFunc db = updateTableInDB db newTable
    where
      newTable = removeAttributeFromTable (getTable db tableName) attributeName


createDrop :: String -> ParserFunction
createDrop commandStr = ParserFunction {fun = boundFunc, funType = "createDrop", args = [tableName]}
  where
  (funName:tableName:xs) = toArgs commandStr
  boundFunc db = dropTableInDB db tableName

createCopy :: String -> ParserFunction
createCopy commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createCopy",
  args = [oldTableName, newTableName]}
  where
  oldTableName:newTableName:xs = extractArgs ["COPY ", " AS "] commandStr

  boundFunc db = insertTableIntoDB db (oldTable{selector = newTableName})
    where
      oldTable = getTable db oldTableName

createRename :: String -> ParserFunction
createRename commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createRename",
  args = [oldTableName, newTableName]}
  where
  oldTableName:newTableName:xs = extractArgs ["RENAME ", " AS "] commandStr

  boundFunc db = insertTableIntoDB dbUpdate (oldTable{selector = newTableName})
    where
      oldTable = getTable db oldTableName
      dbUpdate = dropTableInDB db oldTableName



-- https://stackoverflow.com/questions/49228467/split-string-on-multiple-delimiters-of-any-length-in-haskell
splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

extractArgs :: [String] -> String -> [String]
extractArgs argBreaks commandStr = filter (not . null) (splitOnAnyOf argBreaks commandStr)

validateMergeArgs :: [String] -> [String]
validateMergeArgs args
  | length args < 3 = error "MERGE REQUIRES AT LEAST THREE ARGUMENTS, SEPERATED BY 'AND' and 'AS'"
  | otherwise = args

createMerge :: String -> ParserFunction
createMerge commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createMerge",
  args = foundArgs
  }
  where
    foundArgs = validateMergeArgs (filter (not . null) (splitOnAnyOf ["MERGE ", " AND ", " AS "] commandStr))
    newTableName = last foundArgs
    mergingTables = init foundArgs

    boundFunc db = insertTableIntoDB db mergedTable
      where
        currentTables = map (getTable db) mergingTables
        mergedTable = Table {
          selector = newTableName,
          attributes = attributes (foldr (<>) (emptyTable "FILLTABLE") currentTables),
          order = -1
        }
