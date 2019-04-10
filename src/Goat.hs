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
    putStr "Test"
-- main
--   = do { progname <- getProgName
--         ; args <- getArgs
--         ; checkArgs progname args
--         ; input <- readFile (head args)
--         ; let output = runGoatParse input
--         ; case output of
--             Right ast -> print ast
--             Left  err -> do { putStr "Parse error at "
--                             ; print err
--                             }
--         }

-- checkArgs :: String -> [String] -> IO ()
-- checkArgs _ [filename]
--     = return ()
-- checkArgs progname _
--     = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
--         ; exitWith (ExitFailure 1)
--         }