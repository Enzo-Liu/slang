{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Except
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Short    as BSS
import qualified Data.Text                as T
import           Foreign.Ptr
import           LLVM
import qualified LLVM.AST                 as AST
import           LLVM.Context
import qualified LLVM.ExecutionEngine     as EE
import           LLVM.PassManager
import           SLang
import           System.Console.Haskeline


jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

foreign import ccall "dynamic" haskFun :: FunPtr (IO ()) -> IO ()

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

run :: FunPtr a -> IO ()
run fn = haskFun (castFunPtr fn :: FunPtr (IO ()))

runJIT :: AST.Module -> IO AST.Module
runJIT mod' =
  withContext $ \context ->
  jit context $ \executionEngine ->
    withModuleFromAST context mod' $ \m -> do
      llstr <- moduleLLVMAssembly m
      BS.putStrLn llstr
      EE.withModuleInEngine executionEngine m $ \ee -> do
        mainfn <- EE.getFunction ee "main"
        forM_ mainfn run
        return mod'

emptyModule :: BSS.ShortByteString -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = label }

initModule :: AST.Module
initModule = emptyModule "default slang repl"

codegen :: AST.Module -> SLProgram -> IO AST.Module
codegen mod' program = runJIT newMod
  where
    modn    = compile program
    newMod  = mod' {AST.moduleDefinitions =
                    AST.moduleDefinitions mod' ++ AST.moduleDefinitions modn}

main :: IO ()
main = runInputT defaultSettings (loop initModule)
  where
    loop :: AST.Module -> InputT IO ()
    loop mod' = do
      minput <- getInputLine "SL> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          let f = "interperter"
              prog = parse f (T.pack input)
          lift (codegen mod' prog) >>= loop
