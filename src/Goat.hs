module Main where

import GoatParser
import GoatLexer
import GoatFormatter

import Data.Char
import Text.Parsec
import System.Environment
import System.Exit

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
        ["-st", source_file] ->
          do
            input <- readFile source_file
            let tokens = runGoatLexer source_file input
            putStrLn (show tokens)
        ["-sa", source_file] ->
          do
            input <- readFile source_file
            let tokens = runGoatLexer source_file input
            let ast = runGoatParser tokens
            case ast of
              Right tree -> putStrLn (show tree)
              Left err ->
                do
                  putStr "Parse error: "
                  putStrLn (show err)
                  exitWith (ExitFailure 2)

        ["-p", source_file] ->
          do
            input <- readFile source_file
            let tokens = runGoatLexer source_file input
            let ast = runGoatParser tokens
            case ast of
              Right tree -> runGoatFormatterAndOutput tree
              Left err ->
                do
                  putStr "Parse error: "
                  putStrLn (show err)
                  exitWith (ExitFailure 2)

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