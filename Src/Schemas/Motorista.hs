module Src.Schemas.Motorista (
    Motorista(..),
    cadastraMotorista,
    getBy,
    insereMotorista,
    checkIsEmpty,
    removerMotorista,
    atualizarMotorista
) where


import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import System.IO
import GHC.IO.Handle (hClose)
import Control.Monad (MonadPlus(mzero))
import qualified Data.Vector as V
import Control.Monad (liftM3)


csvPath :: FilePath
csvPath = "./database/motorista.csv"


data Motorista = Motorista{
    cpf :: String,
    cep :: String,
    nome :: String,
    email :: String,
    telefone :: String,
    senha :: String,
    cnh :: String
} deriving(Show, Eq)


instance ToRecord Motorista where
    toRecord (Motorista cpf cep nome email telefone senha cnh) = record
        [ toField cpf
        , toField cep
        , toField nome
        , toField email
        , toField telefone
        , toField senha
        , toField cnh
        ]


instance FromRecord Motorista where
    parseRecord v
        | length v == 7 = Motorista
            <$> v .! 0
            <*> v .! 1
            <*> v .! 2
            <*> v .! 3
            <*> v .! 4
            <*> v .! 5
            <*> v .! 6
        | otherwise = mzero


cadastraMotorista :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
cadastraMotorista cpf cep nome email telefone senha cnh = do
    motoristaCpfExist <- getBy "cpf" cpf
    case motoristaCpfExist of
        Just motorista -> do
            putStrLn "Já existe um motorista cadastrado com esse CPF"
            print motorista --- AQUI IANN
            return Nothing
        Nothing -> do
            motoristaEmailExist <- getBy "email" email
            case motoristaEmailExist of
                Just motorista -> do
                    putStrLn "Já existe um motorista cadastrado com esse Email"
                    print motorista -- AQUI TMB
                    return Nothing
                Nothing -> do
                    motoristaCnhExist <- getBy "cnh" cnh
                    case motoristaCnhExist of
                        Just motorista -> do
                            putStrLn "Já existe um motorista cadastrado com essa CNH"
                            print motorista -- AQUI TMB
                            return Nothing
                        Nothing -> do
                            let novoMotorista = Motorista cpf cep nome email telefone senha cnh
                            insereMotorista novoMotorista
                            putStrLn "Motorista cadastrado com sucesso!"
                            return (Just novoMotorista)


--Função para inserir motorista no banco de dados
insereMotorista :: Motorista -> IO ()
insereMotorista motorista = do
    isEmpty <- checkIsEmpty csvPath
    if isEmpty
        then do
            let csvData = encode [motorista]
                header = B8.pack "cpf,cep,nome,email,telefone,senha,cnh\n"
                final = BL.fromStrict header <> csvData
            withFile csvPath WriteMode $ \handle -> do
                BL.hPutStr handle final
                putStrLn "Motorista e cabeçalho inseridos com sucesso"
        else do
            let csvData = encode [motorista]
            withFile csvPath AppendMode $ \handle -> do
                BL.hPutStr handle csvData
                putStrLn "Motorista inserido com sucesso"


--Carrega o banco de dados de motorista
carregarMotoristas :: FilePath -> IO [Motorista]
carregarMotoristas path = do
    withFile path ReadMode $ \handle -> do
        csvData <- BL.hGetContents handle
        case decode NoHeader csvData of
            Left err -> do
                putStrLn $ "error: " ++ err
                return []
            Right motoristas -> do
                return $ V.toList motoristas


-- Função para buscar valor desejado em uma coluna específica
getBy :: String -> String -> IO (Maybe Motorista)
getBy coluna atributoDesejado = do
    motoristas <- carregarMotoristas csvPath
    let usuarioEncontrado = filter (\u -> getField u == atributoDesejado) motoristas
    case usuarioEncontrado of
        [u] -> return (Just u)
        _ -> do
            return Nothing
    where
        getField motorista =
            case coluna of
                "cpf" -> cpf motorista
                "cep" -> cep motorista
                "nome" -> nome motorista
                "email" -> email motorista
                "telefone" -> telefone motorista
                "senha" -> senha motorista
                "cnh" -> cnh motorista
                _ -> ""


checkIsEmpty :: FilePath -> IO Bool
checkIsEmpty path = do
    withFile path ReadMode $ \handle -> do
        hIsEOF handle


removerMotorista :: String -> IO (Maybe Motorista)
removerMotorista  atributo = do
    motoristas <- carregarMotoristas csvPath
    let motoristasFiltrados = filter (\motorista -> getField motorista /= atributo) motoristas
    if length motoristasFiltrados /= length motoristas
        then do
            escreverMotoristas motoristasFiltrados
            putStrLn "Motorista removido com sucesso."
            return (Just (head motoristasFiltrados)) -- Retornamos Just com qualquer um dos motoristas remanescentes
        else do
            putStrLn "Nenhum motorista atrelado a esse cpf foi encontrado."
            return Nothing
    where
        -- Função auxiliar para obter o valor da coluna cpf, cpf não precisa ser passado como String 
        -- para ser interpretado como coluna
        getField motorista = cpf motorista


escreverMotoristas :: [Motorista] -> IO ()
escreverMotoristas motoristas = do
    let csvData = encode motoristas
    withFile csvPath WriteMode $ \handle -> do
        BL.hPutStr handle csvData


atualizarMotorista :: String -> String -> String -> IO (Maybe Motorista)
atualizarMotorista atributo coluna novoValor = do
    motoristas <- carregarMotoristas csvPath
    let motoristasAtualizados = map (\motorista -> if getField motorista == atributo then atualizarCampo motorista else motorista) motoristas
    if motoristasAtualizados /= motoristas
        then do
            escreverMotoristas motoristasAtualizados
            putStrLn "Motorista atualizado com sucesso."
            return (Just (head motoristasAtualizados)) -- Retornamos Just com o motorista atualizado
        else do
            putStrLn "Nenhum motorista encontrado com o cpf fornecido, ou o valor passado é o mesmo que o atual."
            return Nothing
    where
        getField motorista = cpf motorista
        atualizarCampo motorista =
            case coluna of
                -- CPF NAO PODE SER ATUALIZADO 
                "cep" -> motorista { cep = novoValor }
                -- Nome NAO PODE SER ATUALIZADO
                -- EMAIL NAO PODE SER ATUALIZADO
                "telefone" -> motorista { telefone = novoValor }
                "senha" -> motorista { senha = novoValor }
                -- CNH NAO PODE SER ATUALIZADO
                _ -> motorista


{- confereSenha :: Motorista -> String -> Bool
    confereSenha motorista senhaPassada = senhaPassada == senha motorista -}