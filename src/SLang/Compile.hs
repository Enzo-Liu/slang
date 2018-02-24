{-# LANGUAGE OverloadedStrings #-}

module SLang.Compile where

import           SLang.CodeGen
import           SLang.Expr
import           SLang.Syntax

import           Control.Monad         hiding (void)


import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Short (unpack)
import           Data.Char             (ord)
import qualified LLVM.AST              as AST
import qualified LLVM.AST.Constant     as C
import qualified LLVM.AST.Global       as G
import qualified LLVM.AST.Type         as T

import           LLVM.Context
import           LLVM.Module

codeGen :: Syntax -> LLVM ()
codeGen _ = undefined

cgen :: Syntax -> Codegen AST.Operand
cgen (Call fn args) = do
  let fname = AST.Name fn
      fntype = T.ptr (T.FunctionType void [T.ptr T.i8] False)
  largs <- mapM cgen args
  call (externf fntype fname) largs

cgen (Const cs) = return $ cons $ C.Array T.i8 (map stc (unpack cs))
  where stc = C.Int 8 . toInteger

cgen (Var x) = return $ cons $ C.GetElementPtr True (C.GlobalReference (T.ptr $ T.ArrayType 5 T.i8) (AST.Name x)) [C.Int 32 0, C.Int 32 0]

cgen _ = undefined

main = withContext $ \context -> do
  -- print newast
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    BSC.putStrLn llstr
    return newast
  where
    newast = runLLVM (emptyModule "test") $ do
      addDefn $ AST.GlobalDefinition $ G.globalVariableDefaults {
          G.name  = (AST.Name "t")
        , G.type' = (T.ArrayType 5 T.i8)
        , G.isConstant = True
        , G.initializer = Just (C.Array T.i8
                                (map (C.Int 8 . toInteger . ord) "test\0") )
                                                                }
      external void "puts" [(T.ptr T.i8, AST.UnName 1)]
      define T.i32 "main" [] genBlock

genBlock :: T.Type -> Codegen (AST.Named AST.Terminator)
genBlock _ = do
  let fname = AST.Name "puts"
      fntype = T.ptr (T.FunctionType void [T.ptr T.i8] False)
  largs <- mapM cgen [Var "t"]
  voidcall (externf fntype fname) largs
  ret $ cons (C.Int 32 0)
