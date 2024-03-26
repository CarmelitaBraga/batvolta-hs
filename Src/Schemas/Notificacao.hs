


-- data Notificacao = Notificacao{
--     conteudo :: String
-- } deriving(Show)


-- csvPath :: FilePath
-- csvPath = "./database/notificacao.csv"


-- insereNotificacao :: Notificacao -> IO ()
-- insereNotificacao notificacao = do
--     isEmpty <- checkIsEmpty csvPath
--     if isEmpty
--         then do
--             let csvData = encode [notificacao]
--                 header = B8.pack "conteudo\n"
--                 final = BL.fromStrict header <> csvData
--             withFile csvPath WriteMode $ \handle -> do
--                 BL.hPutStr handle final
--                 putStrLn "Motorista e cabeçalho inseridos com sucesso"
--         else do
--             let csvData = encode [motorista]
--             withFile csvPath AppendMode $ \handle -> do
--                 BL.hPutStr handle csvData
--                 putStrLn "Motorista inserido com sucesso"