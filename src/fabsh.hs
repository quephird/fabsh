import Data.List.Split (splitOn)

import System.Directory (setCurrentDirectory)
import System.Environment (setEnv)
import System.Exit (exitSuccess)
import System.Process (createProcess, proc, waitForProcess)

data Command
  = Cd String
  | Exit
  | Set String String
  | PosixCommand String [String]
  | Echo [String]
  | Unknown


exec :: Command -> IO()
exec (Cd newDir) = setCurrentDirectory newDir
exec Exit        = exitSuccess
exec (Set k v)   = setEnv k v
exec (PosixCommand cmd args) = do
                                 (_, _, _, procHandle) <- createProcess $ proc cmd args
                                 exitCode <- waitForProcess procHandle
                                 return ()
exec _  = putStrLn "I DON'T KNOW WHAT THAT MEANS!"


-- TODO: Improve error handling.
--       Figure out how to implement command recall.
--       ls isn't quite working properly; probably problem with parsing of args
shell :: IO()
shell =
  do
    putStr "💄 👗 👠 💅 >"
    commandAndArgs <- getLine
    let cmd:args   = words commandAndArgs
        fArg:rArgs = args
        k:v:ignore = splitOn "=" fArg in
      do
        case cmd of
          "cd"   -> exec $ Cd fArg
          "exit" -> exec Exit
          "set"  -> exec $ Set k v
          _      -> exec $ PosixCommand cmd args
        shell

main = do
  putStrLn "Welcome to the Fab Shell!!!"
  shell
