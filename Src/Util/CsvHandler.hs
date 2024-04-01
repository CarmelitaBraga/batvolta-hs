module Src.Util.CsvHandler where

import Src.Util.Utils (splitBy)

get :: (String -> t) -> String -> IO [t]
get parser filePath = do
  csvData <- readFile filePath
  seq (length csvData) (return ())
  let lines = splitBy '\n' csvData
  let dataList = map parser lines
  return dataList

append :: (t -> String) -> [t] -> FilePath -> IO ()
append formatter values pathToFile = do
  appendFile pathToFile csvContent
  where
    csvContent = unlines $ map formatter values

write :: (t -> String) -> [t] -> FilePath -> IO ()
write formatter values pathToFile = do
  writeFile pathToFile csvContent
  where
    csvContent = unlines $ map formatter values

delete :: (Show t, Eq t) => (t -> Bool) -> (String -> t) -> (t -> String) -> FilePath -> IO ()
delete predicate parser formatter pathToFile = do
  dataList <- get parser pathToFile
  let updatedDataList = filter (not . predicate) dataList
  write formatter updatedDataList pathToFile
