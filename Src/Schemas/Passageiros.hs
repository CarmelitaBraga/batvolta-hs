{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Src.Schemas.Passageiros (
    Passageiro(..)
    , getPassageiroByCpf
    , cadastraPassageiro
    , confereSenha
    , getCpf
    , removePassageiroByCpf
    , recordToPassageiro
    , csvParseError
    , csvParseDone
    , getPassageiroCSV
    , passageiroObjToCSV
    , passageiroToCSV
    , passageiroListToCSV
    , savePassageiroCSV) where

    import Text.CSV
    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString.Lazy.Char8 as BC
    import GHC.Generics
    import System.IO(openFile, readFile, hClose, IOMode(ReadMode, WriteMode, AppendMode), withFile, hPutStr, hGetContents, hPutStrLn)
    import System.Directory
    import GHC.IO.Handle (hClose)
    import qualified Data.Vector as V
    import Data.Csv (FromRecord(..), ToRecord(..), HasHeader(..), ToField, toRecord, decode, encode, record, toField, (.!))
    import Control.Monad (MonadPlus(mzero))
    

    data Passageiro = Passageiro
        { nome :: String
        , cpf :: String
        , email :: String
        , telefone :: String
        , cep :: String
        , senha :: String
        } deriving (Show, Generic)

    instance ToRecord Passageiro where
        toRecord entry = record
            [ toField (nome entry)
            , toField (cpf entry)
            , toField (email entry)
            , toField (telefone entry)
            , toField (cep entry)
            , toField (senha entry)
            ]
    instance FromRecord Passageiro where
        parseRecord v
            | length v == 6 = Passageiro 
                <$> v .! 0 
                <*> v .! 1 
                <*> v .! 2 
                <*> v .! 3 
                <*> v .! 4 
                <*> v .! 5
            | otherwise = mzero
    

    csvFilePath :: FilePath
    csvFilePath = "./Database/Passageiros.csv"

    cadastraPassageiro :: String -> String -> String -> String -> String -> String -> IO (Maybe Passageiro)
    cadastraPassageiro nome cpf email telefone cep senha = do
        passageiroExist <- getPassageiroByCpf cpf
        case passageiroExist of
            Just _ -> do
                putStrLn "Passageiro já cadastrado"
                return Nothing
            Nothing -> do
                let passageiro = Passageiro { 
                                            nome = nome, 
                                            cpf = cpf, 
                                            email = email, 
                                            telefone = telefone, 
                                            cep = cep,
                                            senha = senha}
                savePassageiroCSV passageiro
                return (Just passageiro)
       
    carregarPassageiros :: IO [Passageiro]
    carregarPassageiros = do
        csvData <- B.readFile csvFilePath
        case decode NoHeader csvData of
            Left err -> do
                putStrLn $ "Erro ao ler o arquivo CSV: " ++ err
                return []  -- Retorna uma lista vazia em caso de erro
            Right passageiros -> return (V.toList passageiros)  -- Converte para lista

    getPassageiroByCpf :: String -> IO (Maybe Passageiro)
    getPassageiroByCpf cpfBuscado = do
        passageiros <- carregarPassageiros
        let passageiroEncontrado = filter (\passageiro -> cpf passageiro == cpfBuscado) passageiros
        return $ case passageiroEncontrado of
            [passageiro] -> Just passageiro
            _ -> Nothing
    
    getPassageiroByEmail :: String -> IO (Maybe Passageiro)
    getPassageiroByEmail emailBuscado = do
        passageiros <- carregarPassageiros
        let passageiroEncontrado = filter (\passageiro -> email passageiro == emailBuscado) passageiros
        return $ case passageiroEncontrado of
            [passageiro] -> Just passageiro
            _ -> Nothing
    
    confereSenha :: Passageiro -> String -> Bool
    confereSenha passageiro senhaPassada = senhaPassada == senha passageiro

    -- Remove passando o cpf como parametro
    removePassageiroByCpf :: String -> [Passageiro] -> [Passageiro]
    removePassageiroByCpf cpf [] = []
    removePassageiroByCpf cpf (x:xs) 
        | getCpf x == cpf = xs
        | otherwise = [x] ++ removePassageiroByCpf cpf xs
    
    recordToPassageiro :: [Record] -> [Passageiro]
    recordToPassageiro [] = []
    recordToPassageiro (x:xs) = do
        let passageiro = Passageiro (x !! 0) (x !! 1) (x !! 2) (x !! 3) (x !! 4) (x !! 5)
        [passageiro] ++ recordToPassageiro xs
    
    csvParseError :: String -> [Record]
    csvParseError err = []

    csvParseDone :: Either String CSV -> [Record]
    csvParseDone (Left err) = []
    csvParseDone (Right csvFile) = tail csvFile


    -- Funções para manipulação do arquivo CSV

    -- Lê o arquivo CSV
    getPassageiroCSV :: FilePath -> IO (Either String CSV)
    getPassageiroCSV path = do
        file <- readFile path
        let csv = parseCSV path file -- definir o parseCSV
        case csv of 
            Left err -> return (Left $ "Não foi possível ler o arquivo CSV: " ++ show err)
            Right csv -> return (Right csv)
    
    passageiroObjToCSV :: Passageiro -> String
    passageiroObjToCSV passageiro = "\n" ++ (nome passageiro) ++ 
                                    "," ++ (cpf passageiro) ++ 
                                    "," ++ (email passageiro) ++ 
                                    "," ++ (telefone passageiro) ++ 
                                    "," ++ (cep passageiro) ++ 
                                    "," ++ (senha passageiro)

    passageiroToCSV :: String -> String -> String -> String -> String -> String -> String
    passageiroToCSV nome cpf email telefone cep senha = "\n" ++ nome ++ 
                                                        "," ++ cpf ++ 
                                                        "," ++ email ++ 
                                                        "," ++ telefone ++ 
                                                        "," ++ cep ++ 
                                                        "," ++ senha

    passageiroListToCSV :: [Passageiro] -> String
    passageiroListToCSV [] = ""
    passageiroListToCSV (x:xs) = passageiroObjToCSV x ++ passageiroListToCSV xs

    -- Salva o passageiro no arquivo CSV
    savePassageiroCSV :: Passageiro -> IO ()
    savePassageiroCSV  passageiro = do
        let fileName = csvFilePath
        withFile fileName AppendMode $ \arq -> do
            B.hPutStr arq $ encode [passageiro]

        -- file <- openFile csvFilePath AppendMode
        -- B.hPutStr file $ encode [passageiro]
        -- hClose file
        -- putStrLn "Passageiro inserido com sucesso!"

    -- Edita o passageiro no arquivo CSV
    -- Busca por cpf e permite edição de cep, telefone e senha
    {- editPassageiroCSV :: FilePath -> String -> String -> String -> String -> IO (Either String ())
    editPassageiroCSV path cpf cep telefone senha = do
        maybePassageiro <- getPassageiroByCpf cpf
        case maybePassageiro of
            Just passageiro -> do
                let passageiroAtualizado = Passageiro (nome passageiro) cpf (email passageiro) telefone cep senha
                let passageiros = removePassageiroByCpf cpf (recordToPassageiro csv)
                let novoCSV = "nome,cpf,email,telefone,cep,senha" ++ passageiroListToCSV (passageiroAtualizado:passageiros)
                writeFile path novoCSV
                return (Right ())
            Nothing -> return (Left "Passageiro não encontrado") -}
               
        

    {- removePassageiroCSV :: FilePath -> String -> IO (Either String ())
    removePassageiroCSV path cpf = do
        csv <- getPassageiroCSV path
        case csv of
            Left err -> return (Left err)
            Right csvFile -> do
                let passageiros = recordToPassageiro csvFile
                let passageirosAtualizados = removePassageiroByCpf cpf passageiros
                let novoCSV = "nome,cpf,email,telefone,cep,senha" ++ passageiroListToCSV passageirosAtualizados
                B.writeFile -}
    
    getCpf :: Passageiro -> String
    getCpf passageiro = cpf passageiro
