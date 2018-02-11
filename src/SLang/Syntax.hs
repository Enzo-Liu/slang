module SLang.Syntax where

import           SLang.Expr
import           Data.ByteString                    (ByteString)

type Symbol = ByteString
type Args = [ByteString]
type Body = [Syntax]
data Syntax = Call Symbol Args | Def Symbol Body deriving Show
