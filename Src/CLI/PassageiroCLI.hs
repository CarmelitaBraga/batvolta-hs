module Src.CLI.PassageiroCLI where

    import Src.Controller.ControllerPassageiro()
    import System.IO
    import Src.Schemas.SchemaPassageiro

    inputString :: String -> IO String
    inputString prompt = do
        putStr prompt
        hFlush stdout
        getLine

    inputInt :: String -> IO Int
    inputInt prompt = do
        str <- inputString prompt
        return (read str)

    menuPrincipal :: IO (Maybe Passageiro)
    menuPrincipal = do
        putStrLn "\nSelecione uma opção:"
        putStrLn "1 - Cadastro de Passageiro"
        putStrLn "2 - Login"
        putStrLn "0 - Sair"
        opcao <- getLine
        case opcao of
            "1" -> menuCadastrarPassageiro
            "2" -> menuRealizarLoginPassageiro
            "0" -> do
                putStrLn "Saindo..."
                return Nothing
            _   -> do
                putStrLn "Opção inválida!"
                menuPrincipal
    
    menuCadastrarPassageiro :: IO (Maybe Passageiro)
    menuCadastrarPassageiro = do
        putStrLn "\nCadastrar Passageiro"
        nome <- inputString "Digite o nome: "
        cpf <- inputString "Digite o CPF: "
        email <- inputString "Digite o e-mail: "
        telefone <- inputString "Digite o telefone: "
        cep <- inputString "Digite o CEP: "
        senha <- inputString "Digite a senha: "
        resultado <- realizarCadastroPassageiro nome email telefone cep senha
        case resultado of
            Just passageiro -> putStrLn "Passageiro cadastrado com sucesso!"
            Nothing -> putStrLn "Erro ao cadastrar passageiro."
        menuPrincipal

    menuAtualizarCadastro :: IO (Maybe Passageiro)
    menuAtualizarCadastro = do
        putStrLn "\nAtualizar Cadastro de Passageiro"
        cpf <- inputString "Digite o CPF do passageiro: "
        putStrLn "Digite os novos valores desejados:"
        cep <- inputString "Digite o novo CEP: "
        telefone <- inputString "Digite o novo telefone: "
        senha <- inputString "Digite a nova senha: "
        atualizarCadastroPassageiro cpf cep telefone senha
        menuOpcoesPassageiro
