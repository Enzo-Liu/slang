{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           LLVM
import qualified LLVM.AST           as AST
import           LLVM.Context
import           SLang
import           System.Directory
import           System.Environment
import           System.IO.Error

main :: IO ()
main = do
  args <- getArgs
  let f = head args
  content <- T.readFile f
  let expr = parse f content
      astModule = compile expr
      (fname, _) = T.breakOnEnd "." (T.pack f)
      targetFile = T.unpack (T.dropEnd 1 fname) <> ".ll"
  removeIfExists targetFile
  toObjectFile (File targetFile) astModule

toObjectFile :: File -> AST.Module -> IO ()
toObjectFile f ast = withContext $ \context ->
  withModuleFromAST context ast $ \m ->
    writeLLVMAssemblyToFile f m

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
