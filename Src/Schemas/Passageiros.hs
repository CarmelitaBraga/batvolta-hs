module Schemas.Passageiro (
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
    , savePassageiroCSV
    , editPassageiroCSV
    , removePassageiroCSV) where

    import System.IO.Unsafe
    import Text.CSV
    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString.Lazy.Char8 as BC
    import GHC.Generics
    import System.IO
    import System.Directory
    import qualified Data.Vector as V
    import Data.Csv (FromRecord(..), ToRecord(..), HasHeader(..), decode)
    

    data Passageiro = Passageiro
        { nome :: String
        , cpf :: String
        , email :: String
        , telefone :: String
        , cep :: String
        , senha :: String
        } deriving (Show, Generic)

    instance FromRecord Passageiro
    instance ToRecord Passageiro

    csvFilePath :: FilePath
    csvFilePath = "./Database/Passageiro.csv"

    cadastraPassageiro :: String -> String -> String -> String -> String -> String -> IO (Maybe Passageiro)
    cadastraPassageiro nome cpf email telefone cep senha = if passageiroExistente cpf email
            then do
                putStrLn "Passageiro já cadastrado"
                return Nothing
            else do
                let passageiro = Passageiro nome cpf email telefone cep senha
                savePassageiroCSV [passageiro]
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
        usuarios <- carregarUsuarios
        let passageiroEncontrado = filter (\passageiro -> getCpf passageiro == cpfBuscado) passageiros
        return $ case passageiroEncontrado of
            [passageiro] -> Just passageiro
            _ -> Nothing
    
    getPassageiroByEmail :: String -> IO (Maybe Passageiro)
    getPassageiroByEmail emailBuscado = do
        passageiros <- carregarPassageiros
        let passageiroEncontrado = filter (\passageiro -> getEmail passageiro == emailBuscado) passageiros
        return $ case passageiroEncontrado of
            [passageiro] -> Just passageiro
            _ -> Nothing
    
    passageiroExistente :: String -> String -> Bool
    passageiroExistente cpf email = do
        passageiroCpf <- getPassageiroByCpf cpf
        passageiroEmail <- getPassageiroByEmail email
        case passageiroCpf of
            Just _ -> True
            Nothing -> case passageiroEmail of
                Just _ -> True
                Nothing -> False
    
    confereSenha :: Passageiro -> String -> Bool
    confereSenha passageiro senha = senha == getSenha passageiro

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
        arquivo <- readFile path
        let csv = parseCSV path file -- definir o parseCSV
        case csv of 
            Left err -> return (Left $ "Não foi possível ler o arquivo CSV: " ++ err)
            Right csv -> return (Right csv)
    
    passageiroObjToCSV :: Passageiro -> String
    passageiroObjToCSV passageiro = "\n" ++ (nome passageiro) ++ 
                                    "," ++ (cpf passageiro) ++ 
                                    "," ++ (email passageiro) ++ 
                                    "," ++ (telefone passageiro) ++ 
                                    "," ++ (cep passageiro) ++ 
                                    "," ++ (senha passageiro)

    passageiroToCSV :: String -> String -> String -> String -> String -> String -> String
    passageiroToCSV nome cpf email telefone cep senha = "\n" ++ nome ++ "," ++ cpf ++ "," ++ email ++ "," ++ telefone ++ "," ++ cep ++ "," ++ senha

    passageiroListToCSV :: [Passageiro] -> String
    passageiroListToCSV [] = ""
    passageiroListToCSV (x:xs) = passageiroObjToCSV x ++ passageiroListToCSV xs

    -- Salva o passageiro no arquivo CSV
    savePassageiroCSV :: String -> String -> String -> String -> String -> String -> IO ()
    savePassageiroCSV nome cpf email telefone cep senha = do
        let passageiro = passageiroToCSV nome cpf email telefone cep senha
        arquivo <- openFile csvFilePath AppendMode
        hPutStr arquivo passageiro
        hClose arquivo

    -- Edita o passageiro no arquivo CSV
    -- Busca por cpf e permite edição de cep, telefone e senha
    editPassageiroCSV :: FilePath -> String -> String -> String -> String -> IO (Either String ())
    editPassageiroCSV path cpf cep telefone senha = do
        maybePassageiro <- getPassageiroByCpf cpf
        case maybePassageiro of
            Just passageiro -> do
                let passageiroAtualizado = Passageiro (nome passageiro) cpf (email passageiro) telefone cep senha
                let passageiros = removePassageiroByCpf cpf (recordToPassageiro csv)
                let novoCSV = "matricula,nome,cpf,email,telefone,cep,senha" ++ passageiroListToCSV (passageiroAtualizado:passageiros)
                writeFile path novoCSV
                return (Right ())
            Nothing -> return (Left "Passageiro não encontrado")
               
        

    removePassageiroCSV :: FilePath -> String -> IO (Either String ())
    removePassageiroCSV path cpf = do
        csv <- getPassageiroCSV path
        case csv of
            Left err -> return (Left err)
            Right csvFile = do
                let passageiros = recordToPassageiro csvFile
                let passageirosAtualizados = removePassageiroByCpf cpf passageiros
                let novoCSV = "matricula,nome,cpf,email,telefone,cep,senha" ++ passageiroListToCSV passageirosAtualizados
    
    getCpf :: Passageiro -> String
    getCpf passageiro = cpf passageiro

main :: IO ()
main = do
    let passageiroTeste = Passageiro "Jão" "111.111.222-66" "jão@gmail.com" "9999-9999" "12345-678" "senha123"

    cadastraPassageiro  "Jão" "111.111.222-66" "jão@gmail.com" "9999-9999" "12345-678" "senha123"