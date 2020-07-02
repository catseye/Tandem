module Language.Tandem.Pragma where

import Language.Tandem.Rule (Label)

data Pragma = Comment String
            | BatchIO Label Label
