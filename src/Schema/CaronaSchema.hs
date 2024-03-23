import System.IO (readFile)

infoCarona :: Int -> IO String
infoCarona id = do
    csvData <- readFile "~/batvolta-hs/database/caronas.csv"
    let caronas = parseCsv csvData
    let maybeCarona = find (\c -> caronaId c == id) caronas
    case maybeCarona of
        Just carona -> return (informacoesSobreCarona carona)
        Nothing -> return "Carona not found"
