module Src.CLI.PassageiroCLI where

    import System.IO
    import Src.Schemas.Passageiro(Passageiro)
    import Src.Controller.ControllerPassageiro(realizarCadastroPassageiro, cancelarCadastroPassageiro, atualizarCadastroPassageiro,visualizarInfoCadastroPassageiro,realizarLoginPassageiro)
    import Data.IORef
    import Control.Monad
    
    -- Passageiro
    type PassageiroRef = IORef (Maybe Passageiro)

    inputString :: String -> IO String
    inputString prompt = do
        putStr prompt
        hFlush stdout
        getLine

    inputInt :: String -> IO Int
    inputInt prompt = do
        str <- inputString prompt
        return (read str)

    menuPrincipalPassageiro :: IO ()
    menuPrincipalPassageiro = do
        putStrLn "\nSelecione uma opção: "
        putStrLn "1 - Cadastro de Passageiro"
        putStrLn "2 - Login"
        putStrLn "0 - Sair"
        opcao <- getLine
        case opcao of
            "1" -> void $ menuCadastrarPassageiro
            "2" -> do
                passageiroRef <- newIORef Nothing               
                void $ menuRealizarLoginPassageiro passageiroRef
            "0" -> do
                putStrLn "Saindo..."
            _   -> do
                putStrLn "Opção inválida!"
                menuPrincipalPassageiro 

    menuOpcoesPassageiro :: PassageiroRef -> IO ()
    menuOpcoesPassageiro passageiroRef = do
        passageiro <- readIORef passageiroRef
        print passageiro
        putStrLn "\nOpções do Passageiro: "
        putStrLn "1 - Atualizar Cadastro"
        putStrLn "2 - Cancelar Cadastro"
        putStrLn "3 - Visualizar Informações"
        putStrLn "0 - Voltar Menu Principal"
        opcao <- getLine
        case opcao of
            "1" -> void $ menuAtualizarCadastroPassageiro passageiroRef
            "2" -> void $ menuCancelarCadastroPassageiro passageiroRef
            "3" -> void $ menuVisualizarInfoPassageiro passageiroRef
            "0" -> menuPrincipalPassageiro
            _   -> do
                putStrLn "Opção inválida!"
                menuOpcoesPassageiro passageiroRef

    menuCadastrarPassageiro :: IO ()
    menuCadastrarPassageiro = do
        putStrLn "\nCadastrar Passageiro"
        nome <- inputString "Digite o nome: "
        cpf <- inputString "Digite o CPF: "
        email <- inputString "Digite o e-mail: "
        telefone <- inputString "Digite o telefone: "
        cep <- inputString "Digite o CEP: "
        senha <- inputString "Digite a senha: "
        resultado <- realizarCadastroPassageiro nome cpf email telefone cep senha
        case resultado of
            Just passageiro -> putStrLn "Passageiro cadastrado com sucesso!"
            Nothing -> putStrLn "Erro ao cadastrar passageiro."
        menuPrincipalPassageiro

    menuCancelarCadastroPassageiro :: PassageiroRef -> IO ()
    menuCancelarCadastroPassageiro passageiroRef = do
        putStrLn "\nCancelar Cadastro de Passageiro"
        cpf <- inputString "Digite seu CPF: "
        senha <- inputString "Digite sua senha: "
        resultado <- cancelarCadastroPassageiro cpf senha
        case resultado of
            Just passageiro -> do
                putStrLn "Cadastro de passageiro cancelado com sucesso!"
                menuPrincipalPassageiro
            Nothing -> do
                putStrLn "Erro ao cancelar cadastro de passageiro."
                menuOpcoesPassageiro passageiroRef

    menuAtualizarCadastroPassageiro :: PassageiroRef -> IO ()
    menuAtualizarCadastroPassageiro passageiroRef = do
        putStrLn "\nAtualizar Cadastro de Passageiro"
        cpf <- inputString "Digite seu CPF: "
        senha <- inputString "Digite sua senha: "
        putStrLn "Selecione o atributo a ser atualizado: "
        putStrLn "1 - Telefone"
        putStrLn "2 - Cep"
        putStrLn "3 - Senha"
        opcao <- inputString "Opção: "
        novoValor <- inputString "Digite o novo valor: "
        resultado <- case opcao of
            "1" -> atualizarCadastroPassageiro cpf senha "Telefone" novoValor
            "2" -> atualizarCadastroPassageiro cpf senha "Cep" novoValor
            "3" -> atualizarCadastroPassageiro cpf senha "Senha" novoValor
            _   -> return Nothing
        case resultado of
            Just passageiro -> putStrLn "Cadastro de passageiro atualizado com sucesso!"
            Nothing -> putStrLn "Erro ao atualizar cadastro de passageiro."
        menuOpcoesPassageiro passageiroRef

    menuVisualizarInfoPassageiro :: PassageiroRef -> IO ()
    menuVisualizarInfoPassageiro passageiroRef = do
        passageiro <- readIORef passageiroRef
        putStrLn "Informações do passageiro: "
        print passageiro
        menuOpcoesPassageiro passageiroRef
    
    menuRealizarLoginPassageiro :: PassageiroRef -> IO ()
    menuRealizarLoginPassageiro passageiroRef = do
        putStrLn "\nRealizar Login de Passageiro"
        email <- inputString "Digite o e-mail: "
        senha <- inputString "Digite a senha: "
        resultado <- realizarLoginPassageiro email senha
        case resultado of
            Just passageiro -> do
                putStrLn "Login realizado com sucesso!"
                writeIORef passageiroRef resultado
                menuOpcoesPassageiro passageiroRef
            Nothing -> do
                putStrLn "Erro ao realizar login."
                menuPrincipalPassageiro
