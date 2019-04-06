{-# LANGUAGE OverloadedStrings #-}
module SLang where

import           Control.Monad
import qualified Data.Text                  as T
import           LLVM.AST
import           LLVM.AST.Type
import           Text.Parsec
import qualified Text.Parsec.Number         as PN
import           Text.Parsec.Text

import qualified LLVM.IRBuilder.Constant    as LLVMIR
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Module      as LLVMIR
import qualified LLVM.IRBuilder.Monad       as LLVMIR

data SLAtom = SLASymbol T.Text
  | SLString T.Text
  | SLAInt Int
  deriving Show

data SLExpr = SLExpr [SLExpr]
  | SLAtom SLAtom
  deriving Show

parse :: String -> T.Text -> SLExpr
parse fname input = let res = runParser slExpr () fname input
                        in case res of
                             Left err -> error (show err)
                             Right r  -> r

compile :: SLExpr -> Module
compile expr = LLVMIR.buildModule "slang.ll" $ compile' expr

compile' :: LLVMIR.MonadModuleBuilder m => SLExpr -> m Operand
compile' (SLExpr exprs) = compileFunc exprs
compile' (SLAtom atom)  = compileConstant atom

compileFunc :: LLVMIR.MonadModuleBuilder m => [SLExpr] -> m Operand
compileFunc (SLAtom (SLASymbol "+"):args) = do
  unless (length args == 2) (error "wrong number of arguments")
  ops <- mapM compile' args
  let op1 = head ops
      op2 = ops !! 1
  LLVMIR.function "main" [] i32 $ \_ -> do
    op <- LLVMIR.add op1 op2
    LLVMIR.ret op
compileFunc (SLAtom (SLASymbol "-"):args) = undefined
compileFunc _ = undefined

compileConstant :: LLVMIR.MonadModuleBuilder m => SLAtom -> m Operand
compileConstant (SLAInt int) = LLVMIR.int32 $ fromIntegral int
compileConstant _ = undefined

slAInt :: Parser SLAtom
slAInt = SLAInt <$> PN.int

slASymbol :: Parser SLAtom
slASymbol = SLASymbol . T.pack <$> symbol

symbol :: Parser String
symbol = ((:[]) <$> oneOf "+-*/") <|> (many1 letter <> many alphaNum)

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser String
parseString = concat <$> between (char '"') (char '"') (many character)

slString :: Parser SLAtom
slString = SLString . T.pack <$> parseString

slAtom :: Parser SLAtom
slAtom = spaces *> (
  try slAInt  -- if sign is matched, this will fail, so use `try`
  <|> slString
  <|> slASymbol
  ) <* spaces

slExprComposite :: Parser SLExpr
slExprComposite = SLExpr <$>
  between (char '(') (char ')') insideExprs

insideExprs :: Parser [SLExpr]
insideExprs = slExpr `sepBy` spaces

slExpr :: Parser SLExpr
slExpr = slExprComposite <|> SLAtom <$> slAtom
