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

newtype SLProgram = SLProgram [SLExpr] deriving Show

isKey :: T.Text -> SLExpr -> Bool
isKey k (SLExpr (SLAtom (SLASymbol s):_)) | s == k = True
isKey _ _                                 = False

isDef :: SLExpr -> Bool
isDef = isKey "def"
