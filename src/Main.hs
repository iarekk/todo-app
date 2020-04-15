{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.String.Utils(replace)
import Data.List.Safe((!!))
import Data.Time
import Prelude hiding ((!!))
import System.Directory(getHomeDirectory)

import Parsers(runParser)
import Storage(readToDoList, writeToDoList)
import Types

main :: IO ()
main = do
    RunOptions dataPath command <- runParser

    homeDir <- getHomeDirectory

    let expandedDataPath = replace "~" homeDir dataPath
    run expandedDataPath command

run :: FilePath -> Command -> IO ()
run dataPath Info = putStrLn "info"
run dataPath Init = putStrLn "init"
run dataPath List = putStrLn "list"
run dataPath (Add item) = addItem dataPath item
run dataPath (View itemIndex) = viewItem dataPath itemIndex
run dataPath (Update itemIndex itemUpdate) = putStrLn $ "update item #" ++ show itemIndex ++ " with update " ++ show itemUpdate
run dataPath (Remove itemIndex) = removeItem dataPath itemIndex

addItem :: FilePath -> Item -> IO()
addItem path item = do
    ToDoList items <- readToDoList path
    let newToDoList = ToDoList (item : items)
    writeToDoList path newToDoList

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

removeItem :: FilePath -> ItemIndex -> IO()
removeItem path itemIndex = do
    ToDoList items <- readToDoList path
    let mbItems = items `removeAt` itemIndex
    case mbItems of
        Nothing -> putStrLn "Invalid item index"
        Just items' -> do
            let toDoList = ToDoList items'
            writeToDoList path toDoList

removeAt :: [a] -> Int -> Maybe [a]
removeAt xs idx = 
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, _:after) = splitAt idx xs
            xs' = before ++ after
        in Just xs'
