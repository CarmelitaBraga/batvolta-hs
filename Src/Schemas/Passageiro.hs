{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Src.Schemas.Passageiro where

    import Text.CSV
    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString.Lazy.Char8 as BC
    import GHC.Generics
    import System.IO(openFile, readFile, hClose, IOMode(ReadMode, WriteMode, AppendMode), withFile, hPutStr, hGetContents, hPutStrLn)
    import System.Directory
    import GHC.IO.Handle (hIsEOF)
    import qualified Data.Vector as V
    import Data.Csv (FromRecord(..), ToRecord(..), HasHeader(..), ToField, toRecord, decode, encode, record, toField, (.!))
    import Control.Monad (MonadPlus(mzero))
    import Src.Util.Utils (toLowerCase)
    
    data Passageiro = Passageiro
        { nome :: String
        , cpf :: String
        , genero :: String
        , email :: String
        , telefone :: String
        , cep :: String
        , senha :: String
        } deriving (Show, Eq, Generic)

    instance ToRecord Passageiro where
        toRecord entry = record
            [ toField (nome entry)
            , toField (cpf entry)
            , toField (genero entry)
            , toField (email entry)
            , toField (telefone entry)
            , toField (cep entry)
            , toField (senha entry)
            ]

    instance FromRecord Passageiro where
        parseRecord v
            | length v == 7 = Passageiro
                <$> v .! 0
                <*> v .! 1
                <*> v .! 2
                <*> v .! 3
                <*> v .! 4
                <*> v .! 5
                <*> v .! 6
            | otherwise = mzero


    csvFilePath :: FilePath
    csvFilePath = "./database/passageiros.csv"

    cadastraPassageiro :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Passageiro)
    cadastraPassageiro nome cpf genero email telefone cep senha= do
        passageiroExist <- getPassageiroByCpf cpf
        case passageiroExist of
            Just _ -> do
                putStrLn "CPF já cadastrado!"
                return Nothing
            Nothing -> do
                passageiroExist <- getPassageiroByEmail email
                case passageiroExist of
                    Just _ -> do
                        putStrLn "Email já cadastrado!"
                        return Nothing
                    Nothing -> do
                        let generoAlterado = toLowerCase genero
                        let passageiro = Passageiro nome cpf generoAlterado email telefone cep senha
                        savePassageiroCSV passageiro
                        return (Just passageiro)

    carregarPassageiros :: IO [Passageiro]
    carregarPassageiros = do
        withFile csvFilePath ReadMode $ \handle -> do
            csvData <- BC.hGetContents handle
            case decode NoHeader csvData of
                Left err -> do
                    putStrLn $ "error: " ++ err
                    return []
                Right passageiro -> do
                    return $ V.toList passageiro

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
    removePassageiroByCpf :: String -> IO (Maybe Passageiro)
    removePassageiroByCpf cpfBuscado = do
        passageiros <- carregarPassageiros
        let passageirosFiltrados = filter (\passageiro -> getCpf passageiro /= cpfBuscado) passageiros
        if length passageirosFiltrados < length passageiros
            then do
                escreverPassageiros passageirosFiltrados
                return (Just (head passageirosFiltrados))
            else return Nothing

    escreverPassageiros :: [Passageiro] -> IO ()
    escreverPassageiros passageiros = do
        let csvData = encode passageiros
        withFile csvFilePath WriteMode $ \handle -> do
            B.hPutStr handle csvData

    -- Salva o passageiro no arquivo CSV
    savePassageiroCSV :: Passageiro -> IO ()
    savePassageiroCSV  passageiro = do
        let fileName = csvFilePath
        withFile fileName AppendMode $ \arq -> do
            B.hPutStr arq $ encode [passageiro]
            --putStrLn "Passageiro inserido com sucesso!"

    -- Busca por cpf e permite edição de cep, telefone e senha
    editPassageiroCSV :: String ->  String -> String -> IO (Maybe Passageiro)
    editPassageiroCSV cpfBuscado coluna novoValor = do
        passageiros <- carregarPassageiros
        let passageirosAtualizados = map (\passageiro -> if getCpf passageiro == cpfBuscado then atualizarCampo passageiro else passageiro) passageiros
        if passageirosAtualizados /= passageiros
            then do
                escreverPassageiros passageirosAtualizados
                return (Just (head passageirosAtualizados)) -- Retornamos Just com o passageiro atualizado
            else do
                putStrLn "O valor passado é o mesmo que o atual."
                return Nothing
        where
            atualizarCampo passageiro =
                case coluna of
                    -- Nome NAO PODE SER ATUALIZADO
                    -- CPF NAO PODE SER ATUALIZADO 
                    -- EMAIL NAO PODE SER ATUALIZADO
                    "Telefone" -> passageiro { telefone = novoValor }
                    "Cep" -> passageiro { cep = novoValor }
                    "Senha" -> passageiro { senha = novoValor }
                    _ -> passageiro

    
    getCpf :: Passageiro -> String
    getCpf = cpf

    getCLICpf :: Maybe Passageiro -> String
    getCLICpf Nothing = "Passageiro não encontrado"
    getCLICpf (Just passageiro) = cpf passageiro

    checkIsEmpty ::  IO Bool
    checkIsEmpty = do
        withFile csvFilePath ReadMode $ \handle -> do
            hIsEOF handle