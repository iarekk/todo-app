{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either.Combinators (rightToMaybe)
import           Data.String.Utils(replace)
import           Data.List.Safe((!!))
import           Data.Time
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           Options.Applicative hiding (infoParser)
import           Prelude hiding ((!!))
import           System.Directory(getHomeDirectory)
import           System.IO.Error

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority

data Item = Item
    { title         :: ItemTitle
    , description   :: ItemDescription
    , priority      :: ItemPriority
    , dueBy         :: ItemDueBy
    } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

data ItemUpdate = ItemUpdate
    { titleUpdate       :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate    :: Maybe ItemPriority
    , dueByUpdate       :: Maybe ItemDueBy
    } deriving Show

data RunOptions = RunOptions FilePath Command deriving Show

data Command =
    Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving Show

newtype ToDoList = ToDoList [Item] deriving (Generic,Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
    <$> argument str (metavar "TITLE" <> help "title")
    <*> optional itemDescriptionValueParser
    <*> optional itemPriorityValueParser
    <*> optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
    <$> optional updateItemTitleParser
    <*> optional updateItemDescriptionParser
    <*> optional updateItemPriorityParser
    <*> optional updateItemDueByParser

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
    Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-desc")

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser =
    Just <$> itemPriorityValueParser
    <|> flag' Nothing (long "clear-priority")

updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser =
    Just <$> itemDueByValueParser
    <|> flag' Nothing (long "clear-due-by")


removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

commandParser :: Parser Command
commandParser = subparser $ mconcat
    [ command "info" (info infoParser (progDesc "Show info"))
    , command "init" (info initParser (progDesc "Initialise items"))
    , command "list" (info listParser (progDesc "List items"))
    , command "add" (info addParser (progDesc "Add item"))
    , command "view" (info viewParser (progDesc "View item"))
    , command "update" (info updateParser (progDesc "Update item"))
    , command "remove" (info removeParser (progDesc "Remove item"))
    ]

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath 
    <> long "data-path"
    <> short 'p'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemTitleValueParser :: Parser String
itemTitleValueParser = strOption (long "title" <> short 't' <> metavar "TITLE" <> help "title")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

itemPriorityValueParser :: Parser Priority
itemPriorityValueParser =
    option readPriority (long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority")
    where
        readPriority = eitherReader $ \arg ->
            case arg of
                "1" -> Right High
                "2" -> Right Normal
                "3" -> Right Low
                _   -> Left $ "Invalid priority value: " ++ arg

itemDueByValueParser :: Parser LocalTime
itemDueByValueParser =
    option readDateTime (long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due by")
    where
        readDateTime = eitherReader $ \arg ->
            case parseDateTimeMaybe arg of
                (Just dateTime) -> Right dateTime
                Nothing -> Left $ "Date/time must be in " ++ dateTimeFormat ++ " format"
        parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
        dateTimeFormat = "%Y/%m/%d %H:%M:%S"

optionsParser :: Parser RunOptions
optionsParser = RunOptions
    <$> dataPathParser
    <*> commandParser

main :: IO ()
main = do
    RunOptions dataPath command <- execParser (info optionsParser (progDesc "To-do list application"))

    homeDir <- getHomeDirectory

    let expandedDataPath = replace "~" homeDir dataPath
    run expandedDataPath command

readToDoList :: FilePath -> IO ToDoList
readToDoList path = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (rightToMaybe . Yaml.decodeEither' <$> BS.readFile path)
        (\_ -> return $ Just (ToDoList []))
    case mbToDoList of
        Nothing -> error "YAML file is corrupt"
        Just toDoList -> return toDoList

run :: FilePath -> Command -> IO ()
run dataPath Info = putStrLn "info"
run dataPath Init = putStrLn "init"
run dataPath List = putStrLn "list"
run dataPath (Add item) = putStrLn $ "add: " ++ show item
run dataPath (View itemIndex) = viewItem dataPath itemIndex
run dataPath (Update itemIndex itemUpdate) = putStrLn $ "update item #" ++ show itemIndex ++ " with update " ++ show itemUpdate
run dataPath (Remove itemIndex) = putStrLn $ "remove #" ++ show itemIndex

writeToDoList :: FilePath -> ToDoList -> IO()
writeToDoList path todos = BS.writeFile path (Yaml.encode todos)

viewItem :: FilePath -> ItemIndex -> IO ()
viewItem dataPath itemIndex = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! itemIndex :: Maybe Item
    case mbItem of
        Nothing ->   putStrLn "Invalid item index"
        Just item -> showItem itemIndex item

showItem :: ItemIndex -> Item -> IO()
showItem itemIndex (Item title mbDescription mbPriority mbDueBy) = do
    putStrLn $ "#" ++ show itemIndex ++ ": " ++ title
    putStrLn $ showField "Description" show mbDescription
    putStrLn $ showField "Priority" show mbPriority
    putStrLn $ showField "DueBy" (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueBy

showField :: String -> (a -> String) -> Maybe a -> String
showField fieldName f (Just x) = " * " ++ fieldName ++ ": " ++ f x
showField fieldName _ Nothing = " * " ++ fieldName ++ ": " ++ "(not set)"