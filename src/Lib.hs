module Lib
    ( transpile
    ) where

import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Data.Semigroup
import Data.Maybe
import qualified Data.Foldable as F

-- TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES
-- TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES
-- TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES TYPES

-- the primary data type in our system, by default there is a single root table with no selector at the root of the tree
-- corresponds to anything thats not an attribute in css
data Table = Table {
  selector :: String, -- the primary table name
  parentSelectors :: [String], -- unused until the very end when it's filled up while parsing the tree
  attributes :: [Attribute], -- list of css attributes
  maxOrder :: Int, -- ticker used to maintain order of child tables
  tables :: Map.Map String Table, -- hashmap contain keys of selector names and values of tables
  order :: Int -- ticker user by parent to maintain children order
  } deriving (Eq)

-- TODO de-vendor-prefix the ordering
-- the css attribute class
data Attribute = Attribute {
  cssKey :: String,
  cssVal :: String} deriving (Ord, Eq)

-- intermediate function stage
data ParserFunction = ParserFunction {
  fun :: Table -> Table,
  funType :: String,
  args :: [String]
  }

instance Show Attribute where
  show x = indent $ cssKey x ++ ": " ++ cssVal x ++ ";"

instance Show Table where
  show x = if isMediaQuery (selector x) then printMediaQueryTable x else printNormalTable x

instance Ord Table where
  compare a b
    | order a <= order b = LT
    | order a > order b = GT
    | otherwise = GT

instance Show ParserFunction where
  show x = funType x ++ "(" ++ intercalate ", " (args x) ++ ")"

instance Semigroup Table where
  a <> b = containerTable {attributes = mergedAttributes, order = 1 + max (order a) (order b)}
    where
      containerTable = emptyTable (selector a ++ ",\n" ++ selector b)

      merger = foldr (\ atrib acc -> Map.insert (cssKey atrib) (cssVal atrib) acc)
      mergeLeft = merger Map.empty (attributes a)
      toAttribute (k, v) = Attribute{cssKey = k, cssVal = v}
      mergedAttributes = map toAttribute (Map.toList (merger mergeLeft (attributes b)))

-- AUX METHODS FOR DOING SHOWS
-- AUX METHODS FOR DOING SHOWS
-- AUX METHODS FOR DOING SHOWS
-- AUX METHODS FOR DOING SHOWS

-- convert a table into its selector
buildSelector :: Table -> String
buildSelector x = prefix ++ selector x
  where
    filteredParents = reverse $ filter (not . null) (parentSelectors x)
    isPsuedoSelector = take 1 (selector x) == ":"
    conjoiner = if isPsuedoSelector then "" else " "
    parentSelector = unwords filteredParents
    prefix = if 0 == length filteredParents then "" else parentSelector ++ conjoiner

isMediaQuery :: String -> Bool
isMediaQuery str = take 6 str == "@media"

-- print a non-media query table
printNormalTable :: Table -> String
printNormalTable x = formattedSelf ++ intercalate "\n" (prepChildren x)
  where
    formattedSelf = if not (null $ attributes x) then
        buildSelector x ++ " {\n" ++ generateAttributes x ++ "\n}\n" else ""

-- print a media query table
printMediaQueryTable :: Table -> String
printMediaQueryTable x = formattedFirstPart ++ formattedChildren ++ "\n}\n"
  where
    formattedFirstPart = buildSelector x ++ " {\n" ++ generateAttributes x
    formattedChildren = indent $ intercalate "\n" (prepChildren $ removeMediaQueryPrefix x)

-- remove media query prefeixes from a tree
removeMediaQueryPrefix :: Table -> Table
removeMediaQueryPrefix x = x {
    tables = Map.map removeMediaQueryPrefix (tables x),
    parentSelectors = filter (not.isMediaQuery) (parentSelectors x)
    }

-- aux method for previous two methods, prepares children
prepChildren :: Table -> [String]
prepChildren x = map show $ sort $ map snd (Map.toList $ tables x)

-- prepare attribetus in a sesntible way
generateAttributes :: Table -> String
generateAttributes x = intercalate "\n" (map show (sort (attributes x)))

-- indent a collection of lines, can be applied as many times as is necessary
indent :: String -> String
indent str = "  " ++ intercalate "\n  " (filter (not . null) (splitOn "\n" str))


-- "CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS"
-- "CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS"
-- "CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS""CLASS METHODS"

-- insert a attribute row into a table
addAttributeToTable :: Attribute -> Table -> Table
addAttributeToTable atrib tb = tb {
    attributes = (++) (filter (\ x -> cssKey x /= cssKey atrib) (attributes tb)) [atrib]
  }

-- remove a attribute row into a table
removeAttributeFromTable :: Table -> String -> Table
removeAttributeFromTable tb key = tb{attributes = filter (\ x -> cssKey x /= key) (attributes tb)}

-- generate empty table
emptyTable :: String -> Table
emptyTable str = Table {
  selector = str,
  attributes = [],
  order = -1,
  tables = Map.empty,
  maxOrder = 0,
  parentSelectors = []
  }

-- insert a table into another
insertTableIntoTable :: Table -> Table -> Table
insertTableIntoTable containerTab insertTab = containerTab {tables = newTables, maxOrder = tabOrder + 1}
  where
    tabOrder = maxOrder containerTab
    selectorName = selector insertTab
    newTable = insertTab {order = tabOrder}
    newTables = Map.insert selectorName newTable (tables containerTab)

-- update a particular table within another table
updateTableInTable :: Table -> Table -> Table
updateTableInTable container insertTab = container {
  tables = Map.insert (selector insertTab) insertTab (tables container)}

-- remove a particular table within another table
dropTableInTable :: Table -> String -> Table
dropTableInTable tab selectorName = tab {tables = Map.delete selectorName (tables tab)}

-- get a particular table within another table, creates error if not present
getTable :: Table -> String -> Table
getTable tab tableName = fromMaybe (error("Selector \"" ++ tableName ++ "\" not found")) (Map.lookup tableName (tables tab))


nestedUpdateTableInTable :: Table -> Table -> [String] -> Table
nestedUpdateTableInTable container insertTab [] = insertTableIntoTable container insertTab
nestedUpdateTableInTable container insertTab (tableName:xs) = updateTableInTable container updatedTable
  where
    updatedTable = nestedUpdateTableInTable (getTable container tableName) insertTab xs

nestedGetTableInTable :: Table -> [String] -> Table
nestedGetTableInTable container [tableName] = getTable container tableName
nestedGetTableInTable container (tableName:xs) = nestedGetTableInTable (getTable container tableName) xs

nestedDropTableInTable :: Table -> [String] -> Table
nestedDropTableInTable container [tableName] = dropTableInTable container tableName
nestedDropTableInTable container (tableName:xs) = updateTableInTable container updatedTable
  where
    updatedTable = nestedDropTableInTable (getTable container tableName) xs

-- recursively decorate all tables in database with full selector routing
decorateSelectorRoutes :: [String] -> Table -> Table
decorateSelectorRoutes parentSelectors tab = tab {
  tables = Map.map (decorateSelectorRoutes (sel:parentSelectors)) (tables tab),
  parentSelectors = parentSelectors
  }
  where
    sel = selector tab

-- unused but kinda cool
-- mapTables :: (Table -> Table) -> Table -> Table
-- mapTables f tab = f tab {tables = Map.map f (tables tab)}

-- fold across a single level of tables
foldTables :: (Table -> a -> a) -> a -> Table -> a
foldTables f init tab = foldr f init orderedTables
  where
    orderedTables = sort (map snd (Map.toList (tables tab)))


-- MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES
-- MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES
-- MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES MAIN EVENT CYCLES
-- the main function of the system takes in a string of cssql and returns a string of css
transpile :: String -> String
transpile cssqlInput = foldTables (\tab acc -> show tab ++ acc) "" finalTable
  where
    parsedStatements = map parseStatement (cleanIntoStatements cssqlInput)
    computedTable = executeStatements parsedStatements
    finalTable = decorateSelectorRoutes [] computedTable
-- debugging version
-- transpile cssqlInput = intercalate "\n" (map show . parseStatement (cleanIntoStatements cssqlInput))
-- take in parser functions and execute them, state is determined by order
executeStatements :: [ParserFunction] -> Table
executeStatements = foldl (flip fun) (emptyTable "")

-- initial scrubbing of inputs, splits into statmenets and removes comments
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

-- central parsing dispatch
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
     "NEST"   -> createNest commandStr
     "RENAME" -> createRename commandStr
     _        -> error ("Command not recognized: " ++ funcName)
     where
       funcName = head (toArgs commandStr)

-- mostly used for debugging
createIdentity :: String -> ParserFunction
createIdentity input = ParserFunction {fun = boundId, funType = "identity", args = [input]}
  where
    boundId x = x

-- generate a create parser function statement
-- SYNTAX: CREATE SELECTOR <SELECTOR>;
createTable :: String -> ParserFunction
createTable commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createTable",
  args = [tableName]
  }
  where
    (tableName:xs) = extractArgs ["CREATE SELECTOR "] commandStr
    boundFunc db = insertTableIntoTable db (emptyTable tableName)

-- handwavily validate the inputes to create insert
validateInsertArgs :: [String] -> (String, String, String)
validateInsertArgs [tableName, key, val] = (tableName, key, val)
validateInsertArgs x = error ("INSERT REQUIRES AT EXACTLY THREE ARGUMENTS.\nProblem line: " ++ show x)

-- generate a insert parser function statement
-- SYNTAX: INSERT <SELECTOR> (<CSS_KEY>, <CSS_VALUE>);
createInsert :: String -> ParserFunction
createInsert commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createInsert",
  args = [tableName, show (key, val)]
  }
  where
    (tableName, key, val) = validateInsertArgs (extractArgs ["INSERT ", " (", ", ", ")"] commandStr)
    addAtrib = addAttributeToTable (Attribute{cssKey = key, cssVal = val})
    boundFunc db = updateTableInTable db (addAtrib (getTable db tableName))

-- generate a delete parser function statement
-- SYNTAX: DELETE <SELECTOR> <CSS_KEY>;
createDelete :: String -> ParserFunction
createDelete commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createDelete",
  args = [tableName, attributeName]
  }
  where
  (funName:tableName:attributeName:xs) = toArgs commandStr

  boundFunc db = updateTableInTable db newTable
    where
      newTable = removeAttributeFromTable (getTable db tableName) attributeName

-- generate a drop parser function statement
-- SYNTAX: DROP <SELECTOR>;
createDrop :: String -> ParserFunction
createDrop commandStr = ParserFunction {fun = boundFunc, funType = "createDrop", args = [tableName]}
  where
  (funName:tableName:xs) = toArgs commandStr
  boundFunc db = dropTableInTable db tableName

-- generate a copy parser function statement
-- SYNTAX: COPY <SELECTOR> AS <SELECTOR>;
createCopy :: String -> ParserFunction
createCopy commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createCopy",
  args = [oldTableName, newTableName]}
  where
  oldTableName:newTableName:xs = extractArgs ["COPY ", " AS "] commandStr

  boundFunc db = insertTableIntoTable db (oldTable{selector = newTableName})
    where
      oldTable = getTable db oldTableName

-- generate a rename parser function statement
-- SYNTAX: RENAME <SELECTOR> AS <INNERMOST SELECTOR> IN ... IN <OUTERMOST SELECTOR>;
createRename :: String -> ParserFunction
createRename commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createRename",
  args = [oldTableName, newTableName, show targetPath]}
  where
  oldTableName:newTableName:xs = extractArgs ["RENAME ", " AS ", " IN "] commandStr
  targetPath = reverse xs
  oldPath = reverse (oldTableName:xs)
  boundFunc db = result
    where
      -- get old table
      oldTable = nestedGetTableInTable db oldPath
      -- drop it from the db
      updatedDb = nestedDropTableInTable db oldPath
      -- then rename and insert it
      result = nestedUpdateTableInTable updatedDb (oldTable{selector = newTableName}) targetPath


-- breaks up a statement based on a set of breaker strings, validates input
prepareAndValidate :: ([String] -> [String]) -> [String] -> String -> [String]
prepareAndValidate validator splits input = validator (filter (not . null) (splitOnAnyOf splits input))

-- repeated break on
-- https://stackoverflow.com/questions/49228467/split-string-on-multiple-delimiters-of-any-length-in-haskell
splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

-- pull out args for function types that dont have validators
extractArgs :: [String] -> String -> [String]
extractArgs = prepareAndValidate id

-- handwavily validate merge arguments
validateMergeArgs :: [String] -> [String]
validateMergeArgs args
  | length args < 3 = error ("MERGE REQUIRES AT LEAST THREE ARGUMENTS, SEPERATED BY 'AND' and 'AS'.\nError at line: " ++ concat args)
  | otherwise = args

-- generate a merge parser function statement
-- MERGE <SELECTOR> AND <SELECTOR> AND ... AND <SELECTOR> AS <NEW SELECTOR>;
createMerge :: String -> ParserFunction
createMerge commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createMerge",
  args = foundArgs
  }
  where
    foundArgs = prepareAndValidate validateMergeArgs ["MERGE ", " AND ", " AS "] commandStr
    newTableName = last foundArgs
    mergingTables = init foundArgs

    boundFunc db = insertTableIntoTable db mergedTable
      where
        currentTables = map (getTable db) mergingTables
        tempTable = foldr (<>) (emptyTable "FILLTABLE") currentTables
        mergedTable = tempTable {selector = newTableName, order = -1}


-- handwavily validate nest arguments
validateNestArgs :: [String] -> [String]
validateNestArgs args
  | length args < 2 = error ("NEST REQUIRES AT LEAST TWO ARGUMENTS, SEPERATED BY 'INTO' and 'IN'.\nError at line: " ++ concat args)
  | otherwise = args

-- generate a nest parser function statement
-- NEST <SELECTOR> INTO <INNERMOST SELECTOR> IN ... IN <OUTERMOST SELECTOR>;
createNest :: String -> ParserFunction
createNest commandStr = ParserFunction {
  fun = boundFunc,
  funType = "createNest",
  args = [moveTableName, show containingTableNames]
  }
  where
    foundArgs = prepareAndValidate validateNestArgs ["NEST ", " INTO ", " IN "] commandStr
    moveTableName = head foundArgs
    containingTableNames = reverse (tail foundArgs)

    boundFunc db = dropTableInTable updatedContainer moveTableName
      where
        nestingTable = getTable db moveTableName
        updatedContainer = nestedUpdateTableInTable db nestingTable containingTableNames
