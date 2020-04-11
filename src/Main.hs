module Main(main) where

import Options.Applicative hiding (infoParser)

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe String
type ItemDueBy = Maybe String

data Item = Item
    { title         :: ItemTitle
    , description   :: ItemDescription
    , priority      :: ItemPriority
    , dueBy         :: ItemDueBy
    } deriving Show

data ItemUpdate = ItemUpdate
    { titleUpdate       :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate    :: Maybe ItemPriority
    , dueByUpdate       :: Maybe ItemDueBy
    } deriving Show

data Options = Options FilePath Command deriving Show

data Command =
    Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving Show

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

itemPriorityValueParser :: Parser String
itemPriorityValueParser = strOption (long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority")

itemDueByValueParser :: Parser String
itemDueByValueParser = strOption (long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due by")

optionsParser :: Parser Options
optionsParser = Options
    <$> dataPathParser
    <*> commandParser

main :: IO ()
main = do
    Options dataPath command <- execParser (info (optionsParser) (progDesc "To-do list manager"))
    run dataPath command

run :: FilePath -> Command -> IO ()
run dataPath Info = putStrLn "info"
run dataPath Init = putStrLn "init"
run dataPath List = putStrLn "list"
run dataPath (Add item) = putStrLn $ "add: " ++ show item
run dataPath (View itemIndex) = putStrLn $ "view #" ++ show itemIndex
run dataPath (Update itemIndex itemUpdate) = putStrLn $ "update item #" ++ show itemIndex ++ " with update " ++ show itemUpdate
run dataPath (Remove itemIndex) = putStrLn $ "remove #" ++ show itemIndex

