module SLang.Expr where

import qualified Data.Text as T

type Symbol = T.Text
type Args = [Symbol]
type Params = [SLExpr]

data SLExpr =
  SLFunction Symbol Args [SLExpr]
  | SLCall Symbol Params
  | SLSymbol T.Text
  | SLInt Int
  | SLString T.Text
  deriving Show

newtype SLProgram = SLProgram [SLExpr] deriving Show

isDefun :: SLExpr -> Bool
isDefun SLFunction{} = True
isDefun _            = False
