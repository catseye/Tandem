module Language.Tandem.Rule where

import Language.Tandem.Collection

data Rule = Zero
          | One
          | RewExact Label String String
          | RewReplace Label String String
          | RewFront Label String String
          | Disj Rule Rule
          | Conj Rule Rule
          | Many Rule
    deriving (Show, Ord, Eq)
