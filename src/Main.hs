module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.Tandem.Rule as Rule
import qualified Language.Tandem.Parser as Parser
import qualified Language.Tandem.Eval as Eval


main = do
    args <- getArgs
    case args of
        ["parse", fileName] -> do
            rule <- loadSource fileName
            putStrLn $ show rule
        ["eval", fileName] -> do
            rule <- loadSource fileName
            putStrLn $ show $ Eval.rewrite rule Rule.emptyCollection
        _ -> do
            abortWith "Usage: tandem (parse|eval) <input-filename>"

loadSource fileName = do
    text <- readFile fileName
    case Parser.parseRule text of
        Right expr -> do
            return expr
        Left error ->
            abortWith $ show error

abortWith msg = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)
