module Language.Tandem.Eval where

import qualified Data.Map as Map

type Label = Char

data Rule = Zero
          | One
          | RewExact Label String String
          | RewReplace Label String String
          | RewFront Label String String
          | Disj Rule Rule
          | Conj Rule Rule
          | Many Rule

type Collection = Map.Map Label String

get l c = Map.findWithDefault "" l c
put l b c = Map.insert l b c

startsWith s "" = True
startsWith "" s = False
startsWith (c1:s1) (c2:s2) = c1 == c2 && startsWith s1 s2

stripFront "" s = s
stripFront s "" = ""
stripFront (c1:s1) (c2:s2) = stripFront s1 s2

rewrite :: Rule -> Collection -> Maybe Collection

rewrite Zero c = Nothing

rewrite One c = Just c

rewrite (RewExact l a b) c =
    if (get l c) == a then Just (put l b c) else Nothing

rewrite (RewReplace l a b) c =
    if startsWith (get l c) a then Just (put l b c) else Nothing

rewrite (RewFront l a b) c =
    if startsWith (get l c) a then Just (put l (b ++ (stripFront a $ get l c)) c) else Nothing

rewrite (Disj r1 r2) c =
    case (rewrite r1 c, rewrite r2 c) of
        (Nothing, Nothing) -> Nothing
        (Just c1, Nothing) -> Just c1
        (Nothing, Just c2) -> Just c2
        (Just c1, Just c2) -> if c1 == c2 then (Just c1) else error "choice of more than one redex"

rewrite (Conj r1 r2) c =
    case (rewrite r1 c) of
        Nothing -> Nothing
        Just c1 ->
            case (rewrite r2 c1) of
                Nothing -> Nothing
                Just c2 -> Just c2

rewrite (Many r) c =
    case (rewrite r c) of
        Nothing -> Just c
        (Just c') -> rewrite (Many r) c'
