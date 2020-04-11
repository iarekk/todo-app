module Main(main) where

import Options.Applicative hiding (infoParser)

type ItemIndex = Int

type ItemDescription = Maybe String

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

data Options = Options FilePath Command deriving Show

data Command =
  Info
  | Init
  | List
  | Add
  | View
  | Update
  | Remove
  deriving Show

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
updateParser = pure Update

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

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc")

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
run dataPath Update = putStrLn "update"
run dataPath Remove = putStrLn "remove"

