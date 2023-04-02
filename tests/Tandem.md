Tests for Tandem
================

This document is written in Falderal 0.14 format.  It intentionally avoids
exercising the non-ASCII parts of Tandem's syntax, for compatibility
with ancient systems that have trouble handling it.  Those parts of
the syntax are tested in the [Tandem Syntax](Tandem Syntax.md) test
suite.

Basic Semantics
---------------

    -> Tests for functionality "Evaluate Tandem Program"

    -> Functionality "Evaluate Tandem Program" is implemented by
    -> shell command "bin/tandem showeval %(test-body-file)"

    0
    ===> Nothing

    Q -> 123
    ===> Just (fromList [("Q","123")])

    Q -> 123 & R -> 456
    ===> Just (fromList [("Q","123"),("R","456")])

    Q -> 1 & Q1 -> 2
    ===> Just (fromList [("Q","2")])

    Q -> 1 & Q2 -> 3
    ===> Nothing

    Q -> 1 | R -> 1
    ???> more than one redex

    Q -> 1 & R... -> 1 | Q1 -> 2 & R... -> 2
    ===> Just (fromList [("Q","1"),("R","1")])

    (Q -> 1 & R... -> 1 | Q1 -> 2 & R... -> 2)*
    ===> Just (fromList [("Q","2"),("R","2")])

Pragmas
-------

    -> Tests for functionality "Run Tandem Program"

    -> Functionality "Run Tandem Program" is implemented by
    -> shell command "bin/tandem run %(test-body-file)"

    {B:I,O}
    {!This ->is "my comment%.}
    (Ia...->... & %O...->...b)*
    <== aaa
    ==> bbb

    {B:B,B}1
    <== Revcat me!
    ==> !em tacveR
