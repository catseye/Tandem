{-# LANGUAGE FlexibleContexts #-}
module Language.Tandem.Parser where

import Text.ParserCombinators.Parsec hiding (label)

import Language.Tandem.Rule
import Language.Tandem.Pragma

tandem = do
    ps <- many pragma
    r <- rule
    return (ps, r)

pragma = do
    keyword "{"
    c <- comment <|> batchIOpragma
    keyword "}"
    return c

comment = do
    keyword "!"
    s <- many $ satisfy (\x -> x /= '}')
    return $ Comment s

batchIOpragma = do
    keyword "B"
    keyword ":"
    i <- label
    keyword ","
    o <- label
    return $ BatchIO i o

rule = disj

disj = do
    r1 <- conj
    r2 <- option Zero (do{ keyword "|"; disj })
    return $ if r2 == Zero then r1 else Disj r1 r2

conj = do
    r1 <- star
    r2 <- option One (do{ keyword "&"; conj })
    return $ if r2 == One then r1 else Conj r1 r2

star = do
    r <- term
    b <- option False (do{ keyword "*"; return True })
    return $ if b then (Many r) else r

term = zero <|> one <|> parenthesized <|> reverseIndividual <|> individual

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
    l <- label
    s <- option "" (quotedString <|> bareWord)
    so <- option False ellipsis
    arrow
    t <- option "" (quotedString <|> bareWord)
    to <- option False ellipsis
    return $ case (so, to) of
       (False, False) -> RewExact l s t
       (True, False) -> RewReplace l s t
       (True, True) -> RewFront l s t

reverseIndividual = do
    keyword "%"
    l <- label
    so <- option False ellipsis
    s <- option "" (quotedString <|> bareWord)
    arrow
    to <- option False ellipsis
    t <- option "" (quotedString <|> bareWord)
    return $ case (so, to) of
       (False, False) -> RewExact l (reverse s) (reverse t)
       (True, False) -> RewReplace l (reverse s) (reverse t)
       (True, True) -> RewFront l (reverse s) (reverse t)

label = quotedString <|> bareLabel

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

parseTandem text = parse tandem "" text
