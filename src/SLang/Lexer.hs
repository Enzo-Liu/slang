{-# LANGUAGE OverloadedStrings #-}
module SLang.Lexer where

import           SLang.Expr

import qualified Data.Text          as T
import           Text.Parsec
import qualified Text.Parsec.Number as PN
import           Text.Parsec.Text

slAInt :: Parser SLAtom
slAInt = SLAInt <$> PN.int

parse :: String -> T.Text -> SLProgram
parse fname input = let res = runParser slProg () fname input
                        in case res of
                             Left err -> error (show err)
                             Right r  -> r

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
slAtom =
  try slAInt  -- if sign is matched, this will fail, so use `try`
  <|> slString
  <|> slASymbol

slExprComposite :: Parser SLExpr
slExprComposite = SLExpr <$>
  between (char '(') (char ')') insideExprs

insideExprs :: Parser [SLExpr]
insideExprs = slExpr `sepBy` spaces

slExpr :: Parser SLExpr
slExpr = withSpaces $ slExprComposite <|> SLAtom <$> slAtom
  where
    withSpaces :: Parser SLExpr -> Parser SLExpr
    withSpaces expr = spaces *> expr <* spaces

slProg :: Parser SLProgram
slProg = SLProgram <$> many1 slExpr
