-- module Src.Schema.Database (
--     writeCSV,
--     readCSV
-- ) where

-- import Data.Csv (FromNamedRecord, ToNamedRecord, decodeByName, encodeByName)
-- import Data.ByteString.Lazy (ByteString)
-- import Data.Vector (Vector)

-- -- Define your CSV data type here
-- newtype CSVData = CSVData {
--     text :: String
-- } deriving (Show, Eq)

-- -- Read a CSV file into a list of CSVData
-- readCSV :: FilePath -> IO (Either String (Vector CSVData))
-- readCSV filePath = do
--     csvData <- readFile filePath
--     return $ decodeByName csvData

-- -- Write a list of CSVData to a CSV file
-- writeCSV :: FilePath -> Vector CSVData -> IO ()
-- writeCSV filePath csvData = do
--     let csvEncoded = encodeByName csvData
--     writeFile filePath csvEncoded
