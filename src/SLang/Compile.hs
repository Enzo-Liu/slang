{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module SLang.Compile where

import           LLVM.AST
import           LLVM.AST.Type             as AST
import           SLang.Expr

import           Control.Monad
import qualified LLVM.IRBuilder.Constant    as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR


compile :: SLExpr -> Module
compile expr = IR.buildModule "slang.ll" $ mdo
  printf <- IR.extern "printf" [ptr i8, i32] AST.void
  putInt32 <- IR.function "putInt32" [(i32, "a")] AST.void $ \ops ->
    putsInt printf (head ops)
  IR.function "main" [] i32 $ \_ -> do
    op <- compile' expr
    _ <- IR.call putInt32 [(op, [])]
    ret <- IR.int32 0
    IR.ret ret

compile' :: IR.MonadIRBuilder m => SLExpr -> m Operand
compile' (SLExpr exprs) = compileFunc exprs
compile' (SLAtom atom)  = compileConstant atom

putsInt :: (IR.MonadModuleBuilder m, IR.MonadIRBuilder m) => Operand -> Operand -> m ()
putsInt printf op = do
  -- "%d" = 37,100 = 00100101,01100100 = 9572 = 37 << 8 + 100
  intFormat <- IR.globalStringPtr "%d" "intFormat"
  _ <- IR.call printf [(intFormat, []), (op,[])]
  IR.retVoid

compileFunc :: IR.MonadIRBuilder m => [SLExpr] -> m Operand
compileFunc (SLAtom (SLASymbol "+"):args) = do
  unless (length args == 2) (error "wrong number of arguments")
  ops <- mapM compile' args
  let op1 = head ops
      op2 = ops !! 1
  IR.add op1 op2
compileFunc (SLAtom (SLASymbol "-"):args) = undefined
compileFunc _ = undefined

compileConstant :: Applicative m => SLAtom -> m Operand
compileConstant (SLAInt int) = IR.int32 $ fromIntegral int
compileConstant _            = undefined
