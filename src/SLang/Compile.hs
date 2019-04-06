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


compile :: SLProgram -> Module
compile (SLProgram exprs) = IR.buildModule "slang.ll" $ mdo
  printf <- IR.extern "printf" [ptr i8, i32] AST.void
  putInt32 <- IR.function "putInt32" [(i32, "a")] AST.void $ \ops ->
    putsInt printf (head ops)
  let defs = filter isDef exprs
      instructions = filter (not . isDef) exprs
  IR.function "main" [] i32 $ \_ -> do
    mapM_ (\expr ->
             compile' expr >>=
             (\op -> IR.call putInt32 [(op, [])]))
      instructions
    ret <- IR.int32 0
    IR.ret ret

compile' :: IR.MonadIRBuilder m => SLExpr -> m Operand
compile' (SLExpr exprs) = compileFunc exprs
compile' (SLAtom atom)  = compileConstant atom

putsInt :: (IR.MonadModuleBuilder m, IR.MonadIRBuilder m) => Operand -> Operand -> m ()
putsInt printf op = do
  -- "%d" = 37,100 = 00100101,01100100 = 9572 = 37 << 8 + 100
  intFormat <- IR.globalStringPtr "%d\n" "intFormat"
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
compielFunc' "+" (op1:ops) = foldM IR.add op1 ops
compielFunc' "-" [op1] = IR.int32 0 >>= (`IR.sub` op1)
compielFunc' "-" (op1:ops) = foldM IR.sub op1 ops
compielFunc' "*" (op1:ops) = foldM IR.mul op1 ops
compielFunc' "/" (op1:ops@(_:_)) = foldM IR.udiv op1 ops
compielFunc' _ _ = error "not implemented"
