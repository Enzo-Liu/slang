{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module SLang.Compile where

import           SLang.Expr

import           Control.Monad
import           LLVM.AST
import           LLVM.AST.Type              as AST

import qualified Data.Text                  as T

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
compileFunc (SLAtom (SLASymbol fname):args') = do
  ops <- mapM compile' args'
  compielFunc' fname ops
compileFunc _ = error "should not enter"

compileConstant :: Applicative m => SLAtom -> m Operand
compileConstant (SLAInt int) = IR.int32 $ fromIntegral int
compileConstant _            = undefined

compielFunc' ::IR.MonadIRBuilder m => T.Text -> [Operand] -> m Operand
compielFunc' "+" [op1, op2]= IR.add op1 op2
compielFunc' "-" [op1, op2]= IR.sub op1 op2
compielFunc' "*" [op1, op2]= IR.mul op1 op2
compielFunc' "/" [op1, op2]= IR.udiv op1 op2
compielFunc' _ _ = error "not implemented"
