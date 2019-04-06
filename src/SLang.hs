{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module SLang where
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Text                  as T
import           LLVM.AST
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type              as AST
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
compile expr = LLVMIR.buildModule "slang.ll" $ mdo
  printf <- LLVMIR.extern "printf" [ptr i8, i32] AST.void
  putInt32 <- LLVMIR.function "putInt32" [(i32, "a")] AST.void $ \ops ->
    putsInt printf (head ops)
  LLVMIR.function "main" [] i32 $ \_ -> do
    op <- compile' expr
    _ <- LLVMIR.call putInt32 [(op, [])]
    ret <- LLVMIR.int32 0
    LLVMIR.ret ret

compile' :: LLVMIR.MonadIRBuilder m => SLExpr -> m Operand
compile' (SLExpr exprs) = compileFunc exprs
compile' (SLAtom atom)  = compileConstant atom

putsInt :: (LLVMIR.MonadModuleBuilder m, LLVMIR.MonadIRBuilder m) => Operand -> Operand -> m ()
putsInt printf op = do
  -- "%d" = 37,100 = 00100101,01100100 = 9572 = 37 << 8 + 100
  intFormat <- LLVMIR.globalStringPtr "%d" "intFormat"
  _ <- LLVMIR.call printf [(intFormat, []), (op,[])]
  LLVMIR.retVoid

compileFunc :: LLVMIR.MonadIRBuilder m => [SLExpr] -> m Operand
compileFunc (SLAtom (SLASymbol "+"):args) = do
  unless (length args == 2) (error "wrong number of arguments")
  ops <- mapM compile' args
  let op1 = head ops
      op2 = ops !! 1
  LLVMIR.add op1 op2
compileFunc (SLAtom (SLASymbol "-"):args) = undefined
compileFunc _ = undefined

compileConstant :: Applicative m => SLAtom -> m Operand
compileConstant (SLAInt int) = LLVMIR.int32 $ fromIntegral int
compileConstant _            = undefined

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
