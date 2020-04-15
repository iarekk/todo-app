{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import GHC.Generics
import Data.Time

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