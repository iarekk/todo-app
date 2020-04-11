module Main(main) where

import Options.Applicative hiding (infoParser)

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe String
type ItemDueBy = Maybe String

data ItemUpdate = ItemUpdate
    { titleUpdate :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate :: Maybe ItemPriority
    , dueByUpdate :: Maybe ItemDueBy
    } deriving Show

data Options = Options FilePath Command deriving Show

data Command =
    Info
    | Init
    | List
    | Add
    | View
    | Update ItemIndex ItemUpdate
    | Remove
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
addParser = pure Add

viewParser :: Parser Command
viewParser = pure View

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
    <$> optional updateItemTitleParser -- titleUpdate :: Maybe ItemTitle
    <*> optional updateItemDescriptionParser -- descriptionUpdate :: Maybe ItemDescription
    <*> optional updateItemPriorityParser -- priorityUpdate :: Maybe ItemPriority
    <*> optional updateItemDueByParser -- dueByUpdate :: Maybe ItemDueBy

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
removeParser = pure Remove

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
run dataPath Add = putStrLn "add"
run dataPath View = putStrLn "view"
run dataPath (Update itemIndex itemUpdate) = putStrLn $ "update item #" ++ show itemIndex ++ " with update " ++ show itemUpdate
run dataPath Remove = putStrLn "remove"

