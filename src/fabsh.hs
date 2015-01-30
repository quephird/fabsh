import Data.List.Split (splitOn)

import System.Directory (setCurrentDirectory)
import System.Environment (setEnv)
import System.Exit (exitSuccess)
import System.Process (createProcess, proc, waitForProcess)

shell :: IO()
shell =
  do
    putStr "💄 👗 👠 💅 >"
    commandAndArgs <- getLine
    let command:args = words commandAndArgs in
      do
        case command of
          "cd"   -> do
                      setCurrentDirectory $ head args
                      shell
          "exit" -> do
                      exitSuccess
          "set"  -> do
                      let k:v:ignore = splitOn "=" $ head args
                      setEnv k v
                      shell
          _      -> do
                      (_, _, _, procHandle) <- createProcess (proc command args)
                      waitForProcess procHandle
                      shell

main = do
  putStrLn "Welcome to the Fab Shell!!!"
  shell