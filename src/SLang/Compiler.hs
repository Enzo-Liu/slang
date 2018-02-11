{-# LANGUAGE OverloadedStrings #-}
module SLang.Compiler where

import           LLVM.AST
import qualified LLVM.AST                   as AST
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.Global
import           LLVM.AST.Type
import           LLVM.Context

import           LLVM.Module
import           LLVM.Target

import           Data.ByteString.Char8      as BS

term = Do $ Ret Nothing []
ret1 = Do $ Ret (Just $ ConstantOperand $ C.Int 32 1) []

printFunc = functionDefaults {
  returnType = AST.VoidType,
  name = "print",
  basicBlocks = []
}

printType :: Type
printType = FunctionType AST.VoidType [] False

callPrint :: Named Instruction
callPrint = Do $ Call
  Nothing
  CC.C
  []
  -- (Right $ ConstantOperand (C.GlobalReference printType (mkName "print")))
  (Right $ ConstantOperand (C.GlobalReference (ptr printType) (mkName "print")))
  []
  []
  []

mainFunc = functionDefaults {
  returnType = i32,
  name = "main",
  basicBlocks = [BasicBlock (mkName "entry") [callPrint] ret1
                 ]
  }

slangModule :: AST.Module
slangModule = defaultModule {
  moduleName = "slang",
  moduleDefinitions = [ GlobalDefinition printFunc
                      , GlobalDefinition mainFunc
                      ]
                           }

mtf :: AST.Module -> FilePath -> IO ()
mtf m fp = withContext $ \context ->
  withModuleFromAST context m $ \mod -> do
    res <- moduleLLVMAssembly mod
    BS.putStrLn res
    --withHostTargetMachine $ \tm ->
    --  writeTargetAssemblyToFile tm (File fp) mod

main = mtf slangModule "/tmp/test.ll"
