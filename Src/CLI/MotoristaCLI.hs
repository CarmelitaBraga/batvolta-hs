module Src.CLI.MotoristaCLI where

import Src.Controller.ControllerMotorista(realizarCadastroMotorista, cancelarCadastroMotorista, atualizarCadastroMotorista, visualizarInfoCadastroMotorista, realizarLoginMotorista,carregaNotificacoes)
import System.IO
import Src.Model.MotoristaModel(Motorista, getCpf)
import Data.IORef
import Control.Monad
import Data.Maybe (fromMaybe)
import Src.CLI.CaronaCLI (menuPrincipalCaronaMotorista)
--import Main

-- Motorista Logado
type MotoristaRef = IORef (Maybe Motorista)

-- Funções auxiliares para interação com o usuário
inputString :: String -> IO String
inputString prompt = do
    putStr prompt
    hFlush stdout
    getLine

inputInt :: String -> IO Int
inputInt prompt = do
    str <- inputString prompt
    return (read str)

-- Implementação dos menus
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1 - Cadastro de Motorista"
    putStrLn "2 - Login"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> void menuCadastrarMotorista
        "2" -> do
            motoristaRef <- newIORef Nothing
            void $ menuRealizarLogin motoristaRef
        "0" -> do
            putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipal

menuOpcoesMotorista :: MotoristaRef -> IO ()
menuOpcoesMotorista motoristaRef = do
    putStrLn "\nOpções do Motorista:"
    putStrLn "1 - Atualizar Cadastro"
    putStrLn "2 - Cancelar Cadastro"
    putStrLn "3 - Visualizar Informações"
    putStrLn "4 - Carregar historico de Notificações"
    putStrLn "5 - Menu de Caronas"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> void (menuAtualizarCadastro motoristaRef)
        "2" -> void (menuCancelarCadastro motoristaRef)
        "3" -> void (menuVisualizarInfo motoristaRef)
        "4" -> void (menuCarregarNotificacoes motoristaRef)
        "5" -> void (menuPrincipalCaronaMotorista motoristaRef)
        "0" -> menuPrincipal
        _   -> do
            putStrLn "Opção inválida!"
            menuOpcoesMotorista motoristaRef


menuCadastrarMotorista :: IO ()
menuCadastrarMotorista = do
    putStrLn "\nCadastrar Motorista"
    cpf <- inputString "Digite o CPF do motorista: "
    cep <- inputString "Digite o CEP: "
    nome <- inputString "Digite o nome: "
    email <- inputString "Digite o e-mail: "
    telefone <- inputString "Digite o telefone: "
    senha <- inputString "Digite a senha: "
    cnh <- inputString "Digite a CNH: "
    resultado <- realizarCadastroMotorista cpf cep nome email telefone senha cnh
    case resultado of
        Just motorista -> putStrLn "Motorista cadastrado com sucesso!"
        Nothing -> putStrLn "Erro ao cadastrar motorista."
    menuPrincipal

menuCancelarCadastro :: MotoristaRef -> IO ()
menuCancelarCadastro motoristaRef = do
    putStrLn "\nCancelar Cadastro de Motorista"
    motoristaMaybe <- readIORef motoristaRef
    let cpf = getCpf motoristaMaybe
    senha <- inputString "Digite sua senha:"
    resultado <- cancelarCadastroMotorista cpf senha
    case resultado of
        Just motorista -> do 
            putStrLn "Cadastro de motorista cancelado com sucesso!"
            menuPrincipal        
        Nothing -> do
            putStrLn "Erro ao cancelar cadastro de motorista."
            menuOpcoesMotorista motoristaRef
    

menuAtualizarCadastro ::MotoristaRef -> IO ()
menuAtualizarCadastro motoristaRef = do
    putStrLn "\nAtualizar Cadastro de Motorista"
    motoristaMaybe <- readIORef motoristaRef
    let cpf = getCpf motoristaMaybe
    senha <- inputString "Digite sua senha:"
    putStrLn "Selecione o atributo a ser atualizado:"
    putStrLn "1 - Telefone"
    putStrLn "2 - Cep"
    putStrLn "3 - Senha"
    opcao <- inputString "Opção: "
    novoValor <- inputString "Digite o novo valor: "
    resultado <- case opcao of
        "1" -> atualizarCadastroMotorista cpf senha "Telefone" novoValor
        "2" -> atualizarCadastroMotorista cpf senha "Cep" novoValor
        "3" -> atualizarCadastroMotorista cpf senha "Senha" novoValor
        _   -> return Nothing
    case resultado of
        Just motorista -> putStrLn "Cadastro de motorista atualizado com sucesso!"
        Nothing -> putStrLn "Erro ao atualizar cadastro de motorista."
    menuOpcoesMotorista motoristaRef

menuVisualizarInfo :: MotoristaRef -> IO ()
menuVisualizarInfo motoristaRef = do
    putStrLn "\nInformações do motorista:"
    motoristaMaybe <- readIORef motoristaRef
    print motoristaMaybe
    menuOpcoesMotorista motoristaRef


menuRealizarLogin :: MotoristaRef -> IO ()
menuRealizarLogin motoristaRef = do
    putStrLn "\nRealizar Login de Motorista"
    email <- inputString "Digite o e-mail: "
    senha <- inputString "Digite a senha: "
    resultado <- realizarLoginMotorista email senha
    case resultado of
        Just motorista -> do
            putStrLn "Login bem-sucedido!"
            writeIORef motoristaRef resultado
            menuOpcoesMotorista motoristaRef
        Nothing -> do 
            menuPrincipal


menuCarregarNotificacoes :: MotoristaRef -> IO ()
menuCarregarNotificacoes motoristaRef = do
    putStrLn "\nCarregar Notificações do Motorista"
    motoristaMaybe <- readIORef motoristaRef
    let cpfMotorista = getCpf motoristaMaybe
    notificacoes <- carregaNotificacoes cpfMotorista
    putStrLn "Notificações:"
    mapM_ print notificacoes
    menuOpcoesMotorista motoristaRef