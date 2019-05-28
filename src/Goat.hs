-- | Main module of this Goat Compiler
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module Main(
  main
) where

import GoatParser
import GoatLexer
import GoatFormatter
import GoatAnalyzer
import GoatCodeGenerator
import GoatOptimizer
import GoatAST
import OzInstruction
import OzInstructionFormatter

import System.Environment
import System.Exit

-- | Job type
data Job
  = JobToken | JobAST | JobPrettier | JobDAST | JobRawIns | JobIns | JobTrimIns
  deriving (Eq)

-- | Execute lexer, parser ... in order and output the result based on job type
execute :: Job -> String -> IO ()
execute = executeParser

executeParser :: Job -> String -> IO ()
executeParser job source_file
  = do
      input <- readFile source_file
      -- add \n for fix comment
      let tokens = runGoatLexer source_file (input ++ "\n")
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
                    if job == JobPrettier
                      then do 
                        -- preitter
                        runGoatFormatterAndOutput tree
                        return ()
                      else do
                        -- compile
                        executeCompiler job tree
            Left err ->
              do
                putStr "Syntax error: "
                putStrLn (show err)
                exitWith (ExitFailure 2)


executeCompiler :: Job -> GoatProgram -> IO ()
executeCompiler job astTree
  = do
      let semanticResult = runSemanticCheck astTree
      case semanticResult of
        Right decoratedAST ->
          do
            if job == JobDAST
              then do
                putStrLn (show decoratedAST)
                return ()
              else do
                let ins = runCodeGenerator decoratedAST
                let fins = case job of
                            JobRawIns  -> ins
                            JobIns     -> runOptimizer ins
                            JobTrimIns -> removeComments $ runOptimizer ins
                mapM_ putStrLn (map instructionFormatter fins)
                return ()
        Left err ->
          do
            putStrLn (show err)
            exitWith (ExitFailure 3)
      return ()

-- | Main function that handles the execution arguments
main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs

      let usageMsg = "usage: " ++ progname ++ " [-st | -sa | -sd | -p | -d | -r | -h] file"

      case args of

        ["-st", source_file] -> execute JobToken source_file
        ["-sa", source_file] -> execute JobAST source_file
        ["-sd", source_file] -> execute JobDAST source_file
        ["-p",  source_file] -> execute JobPrettier source_file
        ["-d",  source_file] -> execute JobRawIns source_file
        ["-r",  source_file] -> execute JobTrimIns source_file
        ["-h"] ->
          do
            putStrLn usageMsg
            putStrLn ("Options and arguments:")
            putStrLn ("-st    : display secret tokens")
            putStrLn ("-sa    : display secret Abstract Syntax Tree")
            putStrLn ("-sd    : display secret Decorated Abstract Syntax Tree")
            putStrLn ("-p     : pretty print the source file")
            putStrLn ("-d     : debug build (no optimization)")
            putStrLn ("-r     : release build (full optimization and remove comments)")
            putStrLn ("-h     : display the help menu")
            putStrLn ("file   : the file to be processed")
            putStrLn ("")
            putStrLn ("default build (full optimization and keep comments)")
        [source_file] -> execute JobIns source_file
        _ ->
          do
            putStrLn usageMsg
            exitWith (ExitFailure 1)
