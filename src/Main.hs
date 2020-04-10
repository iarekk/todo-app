module Main(main) where

import Options.Applicative

type ItemIndex = Int

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

data Options = Options FilePath ItemIndex deriving Show

dataPathParser :: Parser FilePath
dataPathParser = strOption $
  value defaultDataPath 
  <> long "data-path"
  <> short 'p'
  <> metavar "DATAPATH"
  <> help ("path to data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

optionsParser :: Parser Options
optionsParser = Options
  <$> dataPathParser
  <*> itemIndexParser

main :: IO ()
main = do
  options <- execParser (info (optionsParser) (progDesc "To-do list manager"))
  putStrLn $ "options=" ++ show options
