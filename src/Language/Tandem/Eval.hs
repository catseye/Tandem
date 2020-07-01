module Language.Tandem.Eval where

import Language.Tandem.Rule

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
        Just c1 -> rewrite r2 c1

rewrite (Many r) c =
    case (rewrite r c) of
        Nothing -> Just c
        (Just c') -> rewrite (Many r) c'
