{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.String.Utils(replace)
import System.Directory(getHomeDirectory)

import Commands
import Parsers(runParser)
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

