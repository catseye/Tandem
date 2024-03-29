module Main where

import Haste.DOM (withElems, getValue, setProp)
import Haste.Events (onEvent, MouseEvent(Click))

import qualified Language.Tandem.Collection as Collection
import qualified Language.Tandem.Rule as Rule
import qualified Language.Tandem.Parser as Parser
import qualified Language.Tandem.Eval as Eval


main = withElems ["prog", "result", "run-button"] driver

driver [progElem, resultElem, runButtonElem] =
    onEvent runButtonElem Click $ \_ -> do
        maybeRule <- getProg progElem
        setProp resultElem "textContent" $ case maybeRule of
            Just rule ->
                case Eval.rewrite rule Collection.empty of
                    Just c ->
                        Collection.depict c
                    Nothing ->
                        ""
            Nothing ->
                "<<Error while parsing program!!>>"

getProg progElem = do
    Just text <- getValue progElem
    case Parser.parseTandem text of
        Right (pragmas, rule) -> do
            return $ Just rule
        Left error ->
            return Nothing
