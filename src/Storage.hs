module Storage (readToDoList, readToDoList', writeToDoList) where

import           Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           System.IO.Error

import Types

readToDoList' :: FilePath -> IO (Either Yaml.ParseException ToDoList)
readToDoList' path =
    Yaml.decodeEither' <$> BS.readFile path

readToDoList :: FilePath -> IO ToDoList
readToDoList path = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (Yaml.decodeEither' <$> BS.readFile path)
        (\_ -> return $ Right (ToDoList []))
    case mbToDoList of
        Left _ -> error "YAML file is corrupt"
        Right toDoList -> return toDoList

writeToDoList :: FilePath -> ToDoList -> IO()
writeToDoList path todos = BS.writeFile path (Yaml.encode todos)