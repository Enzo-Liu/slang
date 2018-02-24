module SLang.Expr where

import           Data.ByteString
import           Data.ByteString.Short
import Data.Text

type Ident = ByteString

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom | Comb [SExpr] deriving Show

type Name = ShortByteString

data Expr
  = Int Integer
  | Str Text
  | Var Name
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  deriving (Eq, Ord, Show)
