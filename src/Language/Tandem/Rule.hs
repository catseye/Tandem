module Language.Tandem.Rule where

import qualified Data.Map as Map

type Label = String

data Rule = Zero
          | One
          | RewExact Label String String
          | RewReplace Label String String
          | RewFront Label String String
          | Disj Rule Rule
          | Conj Rule Rule
          | Many Rule
    deriving (Show, Ord, Eq)

type Collection = Map.Map Label String

emptyCollection = Map.empty
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
