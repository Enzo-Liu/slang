{-# LANGUAGE OverloadedStrings #-}

module SLang.Compile where

import           SLang.CodeGen
import           SLang.Expr

import           Control.Monad         hiding (void)


import qualified Data.ByteString.Char8 as BSC
import           Data.Text (unpack)
import           Data.Char             (ord)
import qualified LLVM.AST              as AST
import qualified LLVM.AST.Constant     as C
import qualified LLVM.AST.Global       as G
import qualified LLVM.AST.Type         as T

toSig :: [Name] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: Expr -> LLVM ()
codegenTop (Function name args body) = do
  define double name largs bls
  where
    largs = map (\x -> (double, AST.Name x)) args
    bls _ = do
      forM_ args $ \a -> do
        var <- alloca double
        store var (local void (AST.Name a))
        assign a var
      cgen body >>= ret

codegenTop (Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = define void "main" [] bls
  where bls _ = cgen exp >>= ret

cgen :: Expr -> Codegen AST.Operand
cgen (Call fn args) = do
  let fname = AST.Name fn
      fntype = T.ptr (T.FunctionType void [T.ptr T.i8] False)
  largs <- mapM cgen args
  call (externf fntype fname) largs

cgen (Str cs) = return $ cons $ C.Array T.i8 (map stc (unpack cs))
  where stc = C.Int 8 . toInteger . ord

cgen (Var x) = return $ cons $ C.GetElementPtr True (C.GlobalReference (T.ptr $ T.ArrayType 5 T.i8) (AST.Name x)) [C.Int 32 0, C.Int 32 0]

cgen _ = undefined

puts :: Name -> Codegen ()
puts n = do
  let fname = AST.Name "puts"
      fntype = T.ptr (T.FunctionType void [T.ptr T.i8] False)
  largs <- mapM cgen [Var n]
  voidcall (externf fntype fname) largs

genBlock :: T.Type -> Codegen (AST.Named AST.Terminator)
genBlock _ = do
  puts "t"
  ret $ cons (C.Int 32 0)

-- source:
-- (external puts (char*))
-- (puts "sdf")
