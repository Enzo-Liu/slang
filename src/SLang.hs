module SLang where

import qualified Data.Text as T
import Text.Parsec.Text
import Text.Parsec
import qualified Text.Parsec.Number as PN

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
                             Right r -> r

compile :: SLExpr -> String
compile (SLExpr exprs) = compileFunc exprs
compile (SLAtom atom) = compileConstant atom

compileFunc :: [SLExpr] -> String
compileFunc = undefined

compileConstant :: SLAtom -> String
compileConstant = undefined

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
