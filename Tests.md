Tests for Tandem
================

Basic Syntax
------------

    -> Tests for functionality "Parse Tandem Program"

    -> Functionality "Parse Tandem Program" is implemented by
    -> shell command "bin/tandem parse %(test-body-file)"

**0** and **1** are Tandem programs.

    0
    ===> Zero

Bunch of Tandem syntax it should be able to parse.

    Q 0 -> 1 & ("R" "M" -> "N" & Z ""... -> "M" | L"M"…→"N"… | P1→ | (Z→1 | X→)* | Ya…->b*) | Q"->"->"..."
    ===> Disj (Conj (RewExact "Q" "0" "1") (Disj (Conj (RewExact "R" "M" "N") (RewReplace "Z" "" "M")) (Disj (RewFront "L" "M" "N") (Disj (RewExact "P" "1" "") (Disj (Many (Disj (RewExact "Z" "" "1") (RewExact "X" "" ""))) (Many (RewReplace "Y" "a" "b"))))))) (RewExact "Q" "->" "...")

No reverse sugar.

    Mabc→def & Mabc…→def & Mabc…→def…
    ===> Conj (RewExact "M" "abc" "def") (Conj (RewReplace "M" "abc" "def") (RewFront "M" "abc" "def"))

Reverse sugar.

    %Mcba→fed & %M…cba→fed & %M…cba→…fed
    ===> Conj (RewExact "M" "abc" "def") (Conj (RewReplace "M" "abc" "def") (RewFront "M" "abc" "def"))

Basic Semantics
---------------

    -> Tests for functionality "Evaluate Tandem Program"

    -> Functionality "Evaluate Tandem Program" is implemented by
    -> shell command "bin/tandem eval %(test-body-file)"

    0
    ===> Nothing

    Q → 123
    ===> Just (fromList [("Q","123")])

    Q → 123 & R → 456
    ===> Just (fromList [("Q","123"),("R","456")])

    Q → 1 & Q1 → 2
    ===> Just (fromList [("Q","2")])

    Q → 1 & Q2 → 3
    ===> Nothing

    Q → 1 | R → 1
    ???> more than one redex

    Q → 1 & R… → 1 | Q1 → 2 & R… → 2
    ===> Just (fromList [("Q","1"),("R","1")])

    (Q → 1 & R… → 1 | Q1 → 2 & R… → 2)*
    ===> Just (fromList [("Q","2"),("R","2")])
