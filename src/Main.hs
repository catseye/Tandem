module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.Tandem.Rule as Rule
import qualified Language.Tandem.Parser as Parser
import qualified Language.Tandem.Pragma as Pragma
import qualified Language.Tandem.Eval as Eval


main = do
    args <- getArgs
    case args of
        ["parse", fileName] -> do
            (_, rule) <- loadSource fileName
            putStrLn $ show rule
        ["eval", fileName] -> do
            (_, rule) <- loadSource fileName
            case Eval.rewrite rule Rule.emptyCollection of
                Just c -> do
                    putStr $ Rule.depict c
                Nothing -> do
                    return ()
        ["showeval", fileName] -> do
            (_, rule) <- loadSource fileName
            putStrLn $ show $ Eval.rewrite rule Rule.emptyCollection
        ["run", fileName] -> do
            (pragmas, rule) <- loadSource fileName
            initState <- setUpPragmas pragmas Rule.emptyCollection
            case Eval.rewrite rule initState of
                Just result -> tearDownPragmas pragmas result
                Nothing -> exitWith $ ExitFailure 1
        _ -> do
            abortWith "Usage: tandem (parse|eval|run) <input-filename>"

loadSource fileName = do
    text <- readFile fileName
    case Parser.parseTandem text of
        Right (pragmas, rule) -> do
            return (pragmas, rule)
        Left error ->
            abortWith $ show error

abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1

setUpPragmas [] state = do
    return state

setUpPragmas (Pragma.BatchIO inputLabel outputLabel:rest) state = do
    input <- getContents
    setUpPragmas rest $ Rule.put inputLabel input state

setUpPragmas (_:rest) state = do
    setUpPragmas rest state

tearDownPragmas [] state = do
    return ()

tearDownPragmas (Pragma.BatchIO inputLabel outputLabel:rest) state = do
    putStrLn $ reverse $ Rule.get outputLabel state
    tearDownPragmas rest state

tearDownPragmas (_:rest) state = do
    tearDownPragmas rest state
