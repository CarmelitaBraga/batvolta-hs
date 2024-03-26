module Src.Logic.LogicPassageiro where
    import Src.Schemas.SchemaPassageiro (Passageiro, getPassageiroByCpf, cadastraPassageiro, confereSenha, removePassageiroByCpf, editPassageiroCSV)
    import Control.Monad (when)
    import Src.Util.Util(validarCPF, nullOrEmpty,validarEmail)

    cadastraPassageiroLogic :: String -> String -> String -> String -> String -> String -> IO (Maybe Passageiro)
    cadastraPassageiroLogic nome cpf email telefone cep senha
        | nullOrEmpty nome = do
            putStrLn "Nome não pode ser vazio"
            return Nothing
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | validarEmail email = do
            putStrLn "E-mail não atende aos requisitos"
            return Nothing
        | nullOrEmpty telefone = do
            putStrLn "Telefone não pode ser vazio"
            return Nothing
        | nullOrEmpty cep = do
            putStrLn "CEP não pode ser vazio"
            return Nothing
        | nullOrEmpty senha = do
            putStrLn "Senha não pode ser vazio"
            return Nothing
        | otherwise = cadastraPassageiro nome cpf email telefone cep senha

    editPassageiroCSVLogic :: String -> String -> String -> String -> IO (Maybe Passageiro)
    editPassageiroCSVLogic cpf cep telefone senha
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | nullOrEmpty telefone = do
            putStrLn "Telefone não pode ser vazio"
            return Nothing
        | nullOrEmpty cep = do
            putStrLn "CEP não pode ser vazio"
            return Nothing
        | nullOrEmpty senha = do
            putStrLn "Senha não pode ser vazio"
            return Nothing
        | otherwise = editPassageiroCSV cpf telefone cep senha

    removePassageiroByCpfLogic :: String -> IO (Maybe Passageiro)
    removePassageiroByCpfLogic cpf
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | otherwise = removePassageiroByCpf cpf

    getPassageiroByCpfLogic :: String -> IO (Maybe Passageiro)
    getPassageiroByCpfLogic cpf
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | otherwise = do
            resultado <- getPassageiroByCpf cpf
            print resultado
            return Nothing