module SLang.Syntax where

import           Data.ByteString.Short (ShortByteString)

type Symbol = ShortByteString
type Args = [Syntax]
type Body = [Syntax]
data Syntax = Call Symbol Args
            | Def Symbol Body
            | Const ShortByteString
            | Var ShortByteString
            deriving Show
