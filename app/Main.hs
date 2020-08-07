module Main (main) where

import System.IO
import qualified Data.Text as T
import Text.Megaparsec
import Mu.Parser

parseProgram :: T.Text -> IO ()
parseProgram source =
  case runParser program "repl" source of
    Left e -> putStrLn $ errorBundlePretty e
    Right ast -> print ast

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  parseProgram $ T.pack input
  repl

main :: IO ()
main = repl
