{-# LANGUAGE OverloadedStrings #-}
module SLang.Lexer where

import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import           SLang.Expr
import           Text.Parsec
import           Text.Parsec.ByteString



lexer :: ByteString -> SExpr
lexer = undefined

ident :: Parser ByteString
ident = BS.pack <$> ((:) <$> letter <*> many alphaNum)

int :: Parser Integer
int = read <$> many1 digit

atom :: Parser Atom
atom = parseN <|> parseIdent
  where parseN = N <$> int
        parseIdent = I <$> ident

sexpr :: Parser SExpr
sexpr = fmap A atom <|> fmap Comb (many sexpr)
