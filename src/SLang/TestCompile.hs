{-# LANGUAGE OverloadedStrings #-}
module SLang.TestCompile where

import           LLVM.AST                    hiding (functionAttributes, type')
import qualified LLVM.AST                    as AST
import           LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention  as CC
import qualified LLVM.AST.Constant           as C
import           LLVM.AST.FunctionAttribute
import           LLVM.AST.Global
import qualified LLVM.AST.Linkage            as L
import           LLVM.AST.ParameterAttribute
import           LLVM.AST.Type
import           LLVM.Context



import           LLVM.Module
import           LLVM.Target

import           Data.ByteString.Char8       as BS hiding (map)
import           Data.Char

term = Do $ Ret Nothing []
ret1 = Do $ Ret (Just $ ConstantOperand $ C.Int 32 0) []

ascii :: Char -> C.Constant
ascii = C.Int 8 . toInteger . ord

printFunc = functionDefaults {
  returnType = AST.VoidType,
  name = "putchar",
  basicBlocks = []
}

putsFunc = functionDefaults {
  returnType = i32,
  name = "puts",
  parameters = ([Parameter (ptr i8) (UnName 0) [NoCapture]], False),
  functionAttributes = [Right NoUnwind],
  basicBlocks = []
}

printType :: Type
printType = FunctionType AST.VoidType [] False

putsType :: Type
putsType = FunctionType i32 [ptr i8] False

callPutChar :: Char -> Named Instruction
callPutChar c = Do $ Call
  Nothing
  CC.C
  []
  -- (Right $ ConstantOperand (C.GlobalReference printType (mkName "print")))
  (Right $ ConstantOperand (C.GlobalReference (ptr printType) (mkName "putchar")))
  [(ConstantOperand $ ascii c, [])]
  []
  []

puts :: Name -> Named Instruction
puts n = Do $ Call
  Nothing
  CC.C
  []
  -- (Right $ ConstantOperand (C.GlobalReference printType (mkName "print")))
  (Right $ ConstantOperand (C.GlobalReference (ptr putsType) (mkName "puts")))
  [(ConstantOperand C.GetElementPtr {
       C.inBounds = True,
       C.address = C.GlobalReference PointerType {
                                         pointerReferent = ArrayType 12 i8,
                                         pointerAddrSpace = AddrSpace 0
                                                  }
                                     n,
       C.indices = [ C.Int {C.integerBits = 32, C.integerValue = 0}
                   , C.Int {C.integerBits = 32, C.integerValue = 0}]
                    }, [])]
  []
  []

putsByChar :: String -> [Named Instruction]
putsByChar = map callPutChar

mainFunc = functionDefaults {
  returnType = i32,
  name = "main",
  basicBlocks = [BasicBlock (mkName "entry")
                 (puts (mkName "hello"):putsByChar "HelloWorld\n")
                 ret1
                 ]
  }

slangModule :: AST.Module
slangModule = defaultModule {
  moduleName = "slang",
  moduleDefinitions = [ GlobalDefinition globalVariableDefaults {
                          name = mkName "hello",
                          linkage = L.Private,
                          isConstant = True,
                          unnamedAddr = Just GlobalAddr,
                          type' = ArrayType 12 i8,
                          initializer = Just (C.Array
                                              i8
                                              (map ascii "Hello World\0"))
                                               }
                      , GlobalDefinition printFunc
                      , GlobalDefinition mainFunc
                      , GlobalDefinition putsFunc
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
