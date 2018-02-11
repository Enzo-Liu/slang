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
import Data.Char

term = Do $ Ret Nothing []
ret1 = Do $ Ret (Just $ ConstantOperand $ C.Int 32 0) []

ascii :: Char -> C.Constant
ascii = C.Int 32 . toInteger . ord

printFunc = functionDefaults {
  returnType = AST.VoidType,
  name = "putchar",
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
  (Right $ ConstantOperand (C.GlobalReference (ptr printType) (mkName "putchar")))
  [(ConstantOperand $ ascii 'a', [])]
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
    withHostTargetMachine $ \tm -> do
      -- writeLLVMAssemblyToFile (File fp) mod
      writeTargetAssemblyToFile tm (File fp) mod

main = mtf slangModule "/tmp/test.s"
