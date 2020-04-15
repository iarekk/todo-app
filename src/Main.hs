{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either.Combinators (rightToMaybe)
import           Data.String.Utils(replace)
import           Data.List.Safe((!!))
import           Data.Time
import qualified Data.Yaml as Yaml
import           Prelude hiding ((!!))
import           System.Directory(getHomeDirectory)
import           System.IO.Error

import           Parsers(runParser)
import           Types


main :: IO ()
main = do
    RunOptions dataPath command <- runParser

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
run dataPath (Add item) = addItem dataPath item
run dataPath (View itemIndex) = viewItem dataPath itemIndex
run dataPath (Update itemIndex itemUpdate) = putStrLn $ "update item #" ++ show itemIndex ++ " with update " ++ show itemUpdate
run dataPath (Remove itemIndex) = putStrLn $ "remove #" ++ show itemIndex

addItem :: FilePath -> Item -> IO()
addItem path item = do
    ToDoList items <- readToDoList path
    let newToDoList = ToDoList (item : items)
    writeToDoList path newToDoList



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