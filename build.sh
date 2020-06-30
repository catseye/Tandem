#!/bin/sh

PROG=tandem

if command -v ghc >/dev/null 2>&1; then
    echo "building $PROG.exe with ghc"
    (cd src && ghc --make Main.hs -o ../bin/$PROG.exe)
else
    echo "ghc not found, not building $PROG.exe"
fi
