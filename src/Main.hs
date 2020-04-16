{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Control.Monad(forM_)
import Data.String.Utils(replace)
import Data.List.Safe((!!))
import Data.Time
import Prelude hiding ((!!))
import System.Directory(doesFileExist, getHomeDirectory)

import Parsers(runParser)
import Storage(readToDoList, readToDoList', writeToDoList)
import Types

main :: IO ()
main = do
    RunOptions dataPath command <- runParser

    homeDir <- getHomeDirectory

    let expandedDataPath = replace "~" homeDir dataPath
    run expandedDataPath command

run :: FilePath -> Command -> IO ()
run dataPath Info = showInfo dataPath
run dataPath Init = initItems dataPath
run dataPath List = listItems dataPath
run dataPath (Add item) = addItem dataPath item
run dataPath (View itemIndex) = viewItem dataPath itemIndex
run dataPath (Update itemIndex itemUpdate) = updateItem dataPath itemIndex itemUpdate
run dataPath (Remove itemIndex) = removeItem dataPath itemIndex

showInfo :: FilePath -> IO()
showInfo dataPath = do
    putStrLn $ "Data file path: " ++ dataPath
    exists <- doesFileExist dataPath
    if exists
    then do
        mbList <- readToDoList' dataPath
        case mbList of
            Left err -> putStrLn $ "The file is corrupted: " ++ show err
            Right (ToDoList items) -> putStrLn $ "Contains: " ++ show (length items) ++ " items"
    else putStrLn "File does not exist"

initItems :: FilePath -> IO()
initItems path = writeToDoList path (ToDoList [])

addItem :: FilePath -> Item -> IO()
addItem path item = do
    ToDoList items <- readToDoList path
    let newToDoList = ToDoList (item : items)
    writeToDoList path newToDoList

listItems :: FilePath -> IO()
listItems path = do
    ToDoList items <- readToDoList path
    forM_ (zip [0..] items) (uncurry showItem) -- uncurry :: (a -> b -> c) -> (a,b) -> c

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



updateItem :: FilePath -> ItemIndex -> ItemUpdate -> IO()
updateItem path itemIndex update =
    if isUpdateEmpty update
    then putStrLn "No fields set for update"
    else updateItem' path itemIndex update

updateItem' :: FilePath -> ItemIndex -> ItemUpdate -> IO()
updateItem' path itemIndex (ItemUpdate mbTitle mbDescription mbPriority mbDueBy) = do
    ToDoList items <- readToDoList path
    let update (Item title description priority dueBy) =
            Item
                (updateField mbTitle title)
                (updateField mbDescription description)
                (updateField mbPriority priority)
                (updateField mbDueBy dueBy)

        mbItems = updateAt items itemIndex update
    case mbItems of
        Nothing -> putStrLn "Invalid item index"
        Just items' -> writeToDoList path (ToDoList items')

isUpdateEmpty :: ItemUpdate -> Bool
isUpdateEmpty (ItemUpdate Nothing Nothing Nothing Nothing) = True
isUpdateEmpty _ = False

updateField :: Maybe a -> a -> a
updateField (Just newValue) _ = newValue
updateField Nothing oldValue = oldValue

updateAt :: [a] -> Int -> (a -> a) -> Maybe [a]
updateAt xs idx f =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, x : after) = splitAt idx xs
            xs' = before ++ f x : after
        in Just xs'

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
        let (before, _ : after) = splitAt idx xs
            xs' = before ++ after
        in Just xs'
