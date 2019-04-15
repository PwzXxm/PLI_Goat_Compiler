-- | Main module of this Goat Compiler
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module Main where

import GoatParser
import GoatLexer
import GoatFormatter

import Data.Char
import Text.Parsec
import System.Environment
import System.Exit

-- | Job type
data Job
  = JobToken | JobAST | JobPrettier | JobCompile
  deriving (Eq)

-- | Execute lexer, parser ... in order and output the result based on job type
execute :: Job -> String -> IO ()
execute job source_file
  = do
      input <- readFile source_file
      let tokens = runGoatLexer source_file input
      if job == JobToken
        then do
          -- tokens from lexer
          putStrLn (show tokens)
          return ()
        else do
          let ast = runGoatParser tokens
          case ast of
            Right tree ->
              do
                if job == JobAST
                  then do
                    -- AST from parser
                    putStrLn (show tree)
                    return ()
                  else do
                    -- preitter
                    runGoatFormatterAndOutput tree
                    return ()
            Left err ->
              do
                putStr "Syntax error: "
                putStrLn (show err)
                exitWith (ExitFailure 2)

-- | Main function that handles the execution arguments
main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs

      let usageMsg = "usage: " ++ progname ++ " [-st | -sa | -p | -h] file"

      case args of

        ["-st", source_file] -> execute JobToken source_file
        ["-sa", source_file] -> execute JobAST source_file
        ["-p", source_file] -> execute JobPrettier source_file
        ["-h"] ->
          do
            putStrLn usageMsg
            putStrLn ("Options and arguments:")
            putStrLn ("-st    : display secret tokens")
            putStrLn ("-sa    : display secret Abstract Syntax Tree")
            putStrLn ("-p     : pretty print the source file")
            putStrLn ("-h     : display the help menu")
            putStrLn ("file   : the file to be processed")
        [source_file] -> 
          do
            putStrLn ("Sorry, cannot generate code yet")
        _ ->
          do
            putStrLn usageMsg
            exitWith (ExitFailure 1)
