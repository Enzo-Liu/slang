module SLang.Expr where

import           Data.ByteString

type Ident = ByteString

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom | Comb [SExpr] deriving Show
