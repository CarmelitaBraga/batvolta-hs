module Motorista (
    Motorista(..),
    insereMotorista,
    getByCpf,
    checkIsEmpty
) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import System.IO
import GHC.IO.Handle (hClose)
import Control.Monad (MonadPlus(mzero))
import qualified Data.Vector as V

csvPath :: FilePath
csvPath = "../../database/motorista.csv"


data Motorista = Motorista{
    cpf :: String,
    cep :: String,
    nome :: String,
    email :: String,
    telefone :: String,
    senha :: String,
    cnh :: String
}

instance ToRecord Motorista where
    toRecord (Motorista cpf cep nome email telefone senha) = record 
        [ toField cpf
        , toField cep
        , toField nome
        , toField email
        , toField telefone
        , toField senha
        ]

instance FromRecord Motorista where
    parseRecord v
        | length v == 6 = Motorista
            <$> v .! 0
            <*> v .! 1
            <*> v .! 2
            <*> v .! 3
            <*> v .! 4
            <*> v .! 5
        | otherwise = mzero

cadastraMotorista :: String -> String -> String -> String -> String -> String -> String -> IO()
cadastraMotorista nome cpf email telefone senha cep cnh = do
    motoristaExist <- getBy "Cpf" cpf
    if (getBy "cpf" cpf && getBy "email" email && getBy "cnh" cnh) then do  
        putStrLn "Informações ja existentes"
    else do
        let novoMotorista = Motorista cpf cep nome email telefone senha cnh
        insereMotorista novoMotorista 
        putStrLn "Motorista cadastrado com sucesso!"
                    
-- Função para inserir motorista no banco de dados
insereMotorista :: Motorista -> IO ()
insereMotorista motorista = do
    let csvData = encode [motorista]
    let header = B8.pack "cpf,cep,nome,email,telefone,senha,cnh\n"
    let final = BL.fromStrict header <> csvData
    isEmpty <- checkIsEmpty csvPath
    if isEmpty then do
        -- Insere no banco de dados
        withFile csvPath WriteMode $ \handle -> do
            BL.hPutStr handle final
        putStrLn "Motorista e cabeçalho inseridos com sucesso"
    else do
    -- Insere no banco de dados sem gerar overwrite dos dados
        withFile csvPath AppendMode $ \handle -> do
            BL.hPutStr handle csvData
            putStrLn "Motorista inserido com sucesso"


-- Função para buscar valor desejado em uma coluna específica


carregarMotoristas :: FilePath -> IO [Motorista]
carregarMotoristas path = do
    csvData <- BL.readFile path
    case decode NoHeader csvData of
        Left err -> do
            putStrLn $ "error: " ++ err
            return []
        Right motoristas -> do
            return $ V.toList motoristas


getBy :: String -> String -> Bool
getby coluna atributoDesejado = do
    motoristas <- carregarMotoristas
    let usuarioEncontrado = filter (\u -> coluna u == atributoDesejado) motoristas
    case usuarioEncontrado of
        [] -> 
            return False
        _ -> 
            return True


checkIsEmpty :: FilePath -> IO Bool
checkIsEmpty path = do
    handle <- openFile path ReadMode
    isEmpty <- hIsEOF handle
    hClose handle
    return isEmpty



        
-- getBy :: String -> String ->  FilePath ->  IO (Maybe Bool)
-- getBy coluna atributoDesejado path = do
--     csvData <- BL.readFile path
--     case decode NoHeader csvData of
--         Left err -> do
--             putStrLn $ "error: " ++ err
--             return False
--         Right motoristas -> do
--             let motoristaLista = V.toList motoristas
--                 motorista = filter (\x -> coluna x == atributoDesejado) motoristaLista
--             if(motorista == null)
--             return True