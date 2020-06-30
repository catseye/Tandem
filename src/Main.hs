module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.Tandem.Parser as Parser
import qualified Language.Tandem.Eval as Eval


main = do
    args <- getArgs
    case args of
        ["parse", fileName] -> do
            expr <- loadSource fileName
            putStrLn $ show expr
        ["eval", fileName] -> do
            expr <- loadSource fileName
            putStrLn $ show $ Eval.evalTandem expr
        _ -> do
            abortWith "Usage: tandem (parse|eval) <input-filename>"

loadSource fileName = do
    text <- readFile fileName
    case Parser.parseTandem text of
        Right expr -> do
            return expr
        Left error ->
            abortWith $ show error

abortWith msg = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)
