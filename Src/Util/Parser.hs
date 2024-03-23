module Utils.Parser where

import Data.List.Split (splitOn)
import Data.List (intercalate, isPrefixOf)

data CSVRow = CSVRow [String] deriving (Show)

parseCSV :: String -> [CSVRow]
parseCSV csvString = map parseRow (lines csvString)
  where
    parseRow :: String -> CSVRow
    parseRow line = CSVRow (splitCSVLine line [])

    splitCSVLine :: String -> String -> [String]
    splitCSVLine "" field = [field]
    splitCSVLine (',':rest) field = field : splitCSVLine rest []
    splitCSVLine ('"':rest) field = case span (/= '"') rest of
        (content, '"':xs) -> splitCSVLine xs (field ++ content ++ "\"")
        _ -> [field]
    splitCSVLine (c:rest) field = splitCSVLine rest (field ++ [c])