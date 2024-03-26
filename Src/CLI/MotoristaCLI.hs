module Src.CLI.MotoristaCLI where

import Src.Controller.ControllerMotorista(realizarCadastroMotorista, cancelarCadastroMotorista, atualizarCadastroMotorista, visualizarInfoCadastroMotorista, realizarLoginMotorista)
import System.IO
import Src.Schemas.Motorista(Motorista)

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
menuPrincipal :: IO (Maybe Motorista)
menuPrincipal = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1 - Cadastro de Motorista"
    putStrLn "2 - Login"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuCadastrarMotorista
        "2" -> menuRealizarLogin
        "0" -> do
            putStrLn "Saindo..."
            return Nothing
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipal

menuOpcoesMotorista :: IO (Maybe Motorista)
menuOpcoesMotorista = do
    putStrLn "\nOpções do Motorista:"
    putStrLn "1 - Atualizar Cadastro"
    putStrLn "2 - Cancelar Cadastro"
    putStrLn "3 - Visualizar Informações"
    putStrLn "0 - Voltar Menu Principal"
    opcao <- getLine
    case opcao of
        "1" -> menuAtualizarCadastro
        "2" -> menuCancelarCadastro
        "3" -> menuVisualizarInfo
        "0" -> menuPrincipal
        _   -> do
            putStrLn "Opção inválida!"
            menuOpcoesMotorista


menuCadastrarMotorista :: IO (Maybe Motorista)
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

menuCancelarCadastro :: IO (Maybe Motorista)
menuCancelarCadastro = do
    putStrLn "\nCancelar Cadastro de Motorista"
    cpf <- inputString "Digite o CPF do motorista a ser cancelado: "
    resultado <- cancelarCadastroMotorista cpf
    case resultado of
        Just motorista -> putStrLn "Cadastro de motorista cancelado com sucesso!"
        Nothing -> putStrLn "Erro ao cancelar cadastro de motorista."
    menuPrincipal

menuAtualizarCadastro :: IO (Maybe Motorista)
menuAtualizarCadastro = do
    putStrLn "\nAtualizar Cadastro de Motorista"
    cpf <- inputString "Digite o CPF do motorista: "
    putStrLn "Selecione o atributo a ser atualizado:"
    putStrLn "1 - Telefone"
    putStrLn "2 - Cep"
    putStrLn "3 - Senha"
    opcao <- inputString "Opção: "
    novoValor <- inputString "Digite o novo valor: "
    resultado <- case opcao of
        "1" -> atualizarCadastroMotorista cpf "Telefone" novoValor
        "2" -> atualizarCadastroMotorista cpf "Cep" novoValor
        "3" -> atualizarCadastroMotorista cpf "Senha" novoValor
        _   -> return Nothing
    case resultado of
        Just motorista -> putStrLn "Cadastro de motorista atualizado com sucesso!"
        Nothing -> putStrLn "Erro ao atualizar cadastro de motorista."
    menuOpcoesMotorista

menuVisualizarInfo :: IO (Maybe Motorista)
menuVisualizarInfo = do
    putStrLn "\nVisualizar Informações de Motorista"
    cpf <- inputString "Digite o CPF do motorista: "
    resultado <- visualizarInfoCadastroMotorista cpf
    case resultado of
        Just resultado -> do
            putStrLn "Informações do motorista:"
            putStrLn $ show resultado
        Nothing -> putStrLn "Motorista não encontrado."
    menuOpcoesMotorista

menuRealizarLogin :: IO (Maybe Motorista)
menuRealizarLogin = do
    putStrLn "\nRealizar Login de Motorista"
    email <- inputString "Digite o e-mail: "
    senha <- inputString "Digite a senha: "
    resultado <- realizarLoginMotorista email senha
    case resultado of
        Just motorista -> do
            putStrLn "Login bem-sucedido!"
            menuOpcoesMotorista
        Nothing -> do 
            putStrLn "E-mail ou senha inválidos."
            menuPrincipal


