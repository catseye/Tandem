{-# LANGUAGE FlexibleContexts #-}
module Language.Tandem.Parser where

import Text.ParserCombinators.Parsec

import Language.Tandem.Rule

-- TODO: optional strings
-- TODO: pragmas
-- TODO: asteration
-- TODO: % reverse syntax

rule = disj

disj = do
    r1 <- conj
    r2 <- option Zero (do{ keyword "|"; disj })
    return $ if r2 == Zero then r1 else Disj r1 r2

conj = do
    r1 <- term
    r2 <- option One (do{ keyword "&"; conj })
    return $ if r2 == One then r1 else Conj r1 r2

term = zero <|> one <|> parenthesized <|> individual

zero = do
    keyword "0"
    return Zero

one = do
    keyword "1"
    return One

parenthesized = do
    keyword "("
    r <- rule
    keyword ")"
    return r

individual = do
    l <- (quotedString <|> bareLabel)
    s <- (quotedString <|> bareWord)
    so <- option False ellipsis
    arrow
    t <- (quotedString <|> bareWord)
    to <- option False ellipsis
    return $ case (so, to) of
       (False, False) -> RewExact l s t
       (True, False) -> RewReplace l s t
       (True, True) -> RewFront l s t

arrow = do
   keyword "->" <|> keyword "→"
   return ()

ellipsis = do
   keyword "..." <|> keyword "…"
   return True

--
-- Low level: Concrete things
--

keyword s = do
    try (string s)
    spaces

bareLabel = do
    c <- upper
    spaces
    return [c]

bareWord = do
    s <- many (alphaNum)
    spaces
    return s

quotedString = do
    char '"'
    s <- many $ satisfy (\x -> x /= '"')
    char '"'
    spaces
    return s

--
-- Driver
--

parseRule text = parse rule "" text
