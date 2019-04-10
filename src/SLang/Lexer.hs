{-# LANGUAGE OverloadedStrings #-}
module SLang.Lexer where

import           SLang.Expr

import qualified Data.Text          as T
import           Text.Parsec
import qualified Text.Parsec.Number as PN
import           Text.Parsec.Text

slAInt :: Parser SLExpr
slAInt = SLInt <$> PN.int

slBool :: Parser SLExpr
slBool = SLBool <$>
  (try (string "true" >> return True)
   <|> try (string "false" >> return False))

parse :: String -> T.Text -> SLProgram
parse fname input = let res = runParser slProg () fname input
                        in case res of
                             Left err -> error (show err)
                             Right r  -> r

slASymbol :: Parser SLExpr
slASymbol = SLSymbol . T.pack <$> symbol

symbol :: Parser String
symbol = ((:[]) <$> oneOf "=+-*/") <|> (many1 letter <> many alphaNum)

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

slString :: Parser SLExpr
slString = SLString . T.pack <$> parseString

slAtom :: Parser SLExpr
slAtom =
  try slAInt  -- if sign is matched, this will fail, so use `try`
  <|> try slBool  -- if sign is matched, this will fail, so use `try`
  <|> slString
  <|> slASymbol

slExprComposite :: Parser SLExpr
slExprComposite = between (char '(') (char ')') insideExprs

insideExprs :: Parser SLExpr
insideExprs = do
  sym <- symbol
  parseBySym $ T.pack sym

tSymbol :: Parser T.Text
tSymbol = fmap T.pack symbol

parseBySym :: T.Text -> Parser SLExpr
parseBySym "defun" = SLFunction <$>
  (spaces *> tSymbol) <*>
  (spaces *> args) <*>
  (spaces *> funcBody)
parseBySym "if" = SLIf <$>
  (spaces *> slExpr) <*>
  (spaces *> slExpr) <*>
  (spaces *> slExpr)
parseBySym n       = SLCall n <$> many slExpr

withBrackets :: Parser a -> Parser a
withBrackets = between (char '(') (char ')')

args :: Parser Args
args = withBrackets $ tSymbol `sepBy` spaces

funcBody :: Parser [SLExpr]
funcBody = slExpr `sepBy1` spaces

slExpr :: Parser SLExpr
slExpr = withSpaces $ slExprComposite <|> slAtom
  where
    withSpaces :: Parser SLExpr -> Parser SLExpr
    withSpaces expr = spaces *> expr <* spaces

slProg :: Parser SLProgram
slProg = SLProgram <$> many1 slExpr
