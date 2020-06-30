{-# LANGUAGE FlexibleContexts #-}
module Language.Tandem.Parser where

import Text.ParserCombinators.Parsec

import Language.Tandem.Rule


rule = zero <|> one -- <|> rewExact <|> rewReplace <|> rewFront <|> disj <|> conj <|> many


zero = do
    keyword "0"
    return Zero

one = do
    keyword "1"
    return One

--
-- Low level: Concrete things
--

keyword s = do
    try (string s)
    spaces

--
-- Driver
--

parseRule text = parse rule "" text
