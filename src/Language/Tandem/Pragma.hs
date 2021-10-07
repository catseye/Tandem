module Language.Tandem.Pragma where

import Language.Tandem.Collection (Label)

data Pragma = Comment String
            | BatchIO Label Label
