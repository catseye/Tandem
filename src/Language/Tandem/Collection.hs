module Language.Tandem.Collection where

import qualified Data.Map as Map

type Label = String

type Collection = Map.Map Label String

empty = Map.empty
get l c = Map.findWithDefault "" l c
put l b c = Map.insert l b c
depict c = d $ Map.toAscList c
    where
        d [] = ""
        d ((l, s):rest) = "\"" ++ l ++ "\"=\"" ++ s ++ "\"\n" ++ d rest

startsWith s "" = True
startsWith "" s = False
startsWith (c1:s1) (c2:s2) = c1 == c2 && startsWith s1 s2

stripFront "" s = s
stripFront s "" = ""
stripFront (c1:s1) (c2:s2) = stripFront s1 s2
