#!/bin/sh

if command -v ghc > /dev/null 2>&1; then
    falderal "tests/Tandem Syntax.md" || exit 1
else
    echo "NOTE, ghc not found on executable search path."
    echo "Not running syntax tests in case your Haskell can't handle them."
fi

falderal "tests/Tandem.md" || exit 1
