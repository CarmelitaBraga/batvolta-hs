{- module Src.Model.Motorista where(
    escreverMotoristas,
    confereSenha,
    checkIsEmpty,
    carregarMotoristas
)


escreverMotoristas :: [Motorista] -> IO ()
escreverMotoristas motoristas = do
    let csvData = encode motoristas
    withFile csvPath WriteMode $ \handle -> do
        BL.hPutStr handle csvData



confereSenha :: Usuario -> String -> Bool
    confereSenha usuario senhaPassada = senhaPassada == senha usuario


checkIsEmpty :: FilePath -> IO Bool
checkIsEmpty path = do
    withFile path ReadMode $ \handle -> do
        hIsEOF handle



carregarMotoristas :: FilePath -> IO [Motorista]
carregarMotoristas path = do
    withFile path ReadMode $ \handle -> do
        csvData <- BL.hGetContents handle
        case decode NoHeader csvData of
            Left err -> do
                putStrLn $ "error: " ++ err
                return []
            Right motoristas -> do
                return $ V.toList motoristas -}