{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text          as T
import           LLVM
import qualified LLVM.AST           as AST
import           LLVM.Context
import           SLang
import System.Console.Haskeline
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

runJIT :: AST.Module -> IO ()
runJIT mod' = do
  withContext $ \context ->
    withModuleFromAST context mod' $ \m -> do
      s <- moduleLLVMAssembly m
      BS.putStrLn s

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                 let f = "interperter"
                     expr = parse f (T.pack input)
                     astModule = compile expr
                 lift $ runJIT astModule
                 loop
