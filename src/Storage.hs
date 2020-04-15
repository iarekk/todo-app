module Storage (readToDoList, writeToDoList) where

import           Control.Exception
import qualified Data.ByteString.Char8 as BS
import           Data.Either.Combinators (rightToMaybe)
import qualified Data.Yaml as Yaml
import           System.IO.Error



import Types

readToDoList :: FilePath -> IO ToDoList
readToDoList path = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (rightToMaybe . Yaml.decodeEither' <$> BS.readFile path)
        (\_ -> return $ Just (ToDoList []))
    case mbToDoList of
        Nothing -> error "YAML file is corrupt"
        Just toDoList -> return toDoList

writeToDoList :: FilePath -> ToDoList -> IO()
writeToDoList path todos = BS.writeFile path (Yaml.encode todos)