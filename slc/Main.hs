module Main where

import SLang
import System.Environment
import qualified Data.Text.IO as T

main :: IO ()
main = do
  args <- getArgs
  let f = head args
  content <- T.readFile f
  print $ parse f content
