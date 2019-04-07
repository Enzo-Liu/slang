{-# LANGUAGE OverloadedStrings #-}
module SLang.Expr where

import qualified Data.Text as T

data SLAtom = SLASymbol T.Text
  | SLString T.Text
  | SLAInt Int
  deriving Show

data SLExpr = SLExpr [SLExpr]
  | SLAtom SLAtom
  deriving Show

symbolName :: SLExpr -> T.Text
symbolName (SLAtom (SLASymbol t)) = t
symbolName _ = error "wrong type to ask for symbol name"

subExprs :: SLExpr -> [SLExpr]
subExprs (SLExpr exprs) = exprs
subExprs _ = error "wrong type to ask for subExprs"

newtype SLProgram = SLProgram [SLExpr] deriving Show

isKey :: T.Text -> SLExpr -> Bool
isKey k (SLExpr (SLAtom (SLASymbol s):_)) | s == k = True
isKey _ _                                 = False

isDef :: SLExpr -> Bool
isDef = isKey "def"

isSymbolDef :: SLExpr -> Bool
isSymbolDef (SLAtom (SLASymbol s)) = s == "def"
isSymbolDef _ = False
