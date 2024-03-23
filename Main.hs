
module Main where
import qualified Motorista as SM


main :: IO ()
main = putStrLn "Hello, Haskell!"

getByCpf :: String -> IO (Maybe SM.Motorista)
