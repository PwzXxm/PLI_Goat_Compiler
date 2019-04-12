module Main where

import GoatParser
import GoatLexer
import GoatFormatter

import Data.Char
import Text.Parsec
import System.Environment
import System.Exit

data Job
  = JobToken | JobAST | JobPrettier | JobCompile
  deriving (Eq)


execute :: Job -> String -> IO ()
execute job source_file
  = do
      input <- readFile source_file
      let tokens = runGoatLexer source_file input
      if job == JobToken
        then do
          putStrLn (show tokens)
          return ()
        else do
          let ast = runGoatParser tokens
          case ast of
            Right tree ->
              do
                if job == JobAST
                  then do
                    putStrLn (show tree)
                    return ()
                  else do
                    runGoatFormatterAndOutput tree
                    return ()
            Left err ->
              do
                putStr "Parse error: "
                putStrLn (show err)
                exitWith (ExitFailure 2)


main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs

      let usageMsg = "usage: " ++ progname ++ " [-st | -sa | -p | -h] file"

      case args of
        [source_file] -> 
          do
            putStrLn ("Sorry, cannot generate code yet")
            putStrLn (usageMsg)

        ["-st", source_file] -> execute JobToken source_file
        ["-sa", source_file] -> execute JobAST source_file
        ["-p", source_file] -> execute JobPrettier source_file
        ["-h", source_file] ->
          do
            putStrLn usageMsg
            putStrLn ("Options and arguments:")
            putStrLn ("-st    : display secret tokens")
            putStrLn ("-sa    : display secret Abstract Syntax Tree")
            putStrLn ("-p     : pretty print the source file")
            putStrLn ("-h     : display the help menu")
            putStrLn ("file   : the file to be processed")
        _ ->
          do
            putStrLn usageMsg
            exitWith (ExitFailure 1)