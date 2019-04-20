module SLang.Expr where

import qualified Data.Text as T

type Symbol = T.Text
type Args = [Symbol]
type Params = [SLExpr]

data SLExpr =
  SLFunction Symbol Args [SLExpr]
  | SLCall Symbol Params
  | SLIf SLExpr SLExpr SLExpr
  | SLSymbol T.Text
  | SLBool Bool
  | SLInt Int
  | SLString T.Text
  | SLDefine Symbol SLExpr
  | SLLambda Args [SLExpr]
  deriving Show

newtype SLProgram = SLProgram [SLExpr] deriving Show

isDefun :: SLExpr -> Bool
isDefun SLFunction{} = True
isDefun _            = False
