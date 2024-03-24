module Src.Util.CsvHandler where

import Src.Util.Utils (splitBy)

append :: (Show t) => [t] -> FilePath -> IO ()
append values pathToFile = do
  appendFile pathToFile csvContent
  where
    csvContent = unlines $ map show values

get :: (String -> t) -> String -> IO [t]
get parser filePath = do
  csvData <- readFile filePath
  seq (length csvData) (return ())
  let lines = splitBy '\n' csvData
  let dataList = map parser lines
  return dataList

write :: (Show t) => [t] -> FilePath -> IO ()
write values pathToFile = do
  writeFile pathToFile csvContent
  where
    csvContent = unlines $ map show values

delete :: (Show t, Eq t) => (t -> Bool) -> (String -> t) -> FilePath -> IO ()
delete predicate parser pathToFile = do
  dataList <- get parser pathToFile
  let updatedDataList = filter (not . predicate) dataList
  write updatedDataList pathToFile
