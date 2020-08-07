module Main (main) where

import System.IO
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Megaparsec
import Mu.Parser
import Mu.Evaluator
import Mu.Util

run :: Aliases -> T.Text -> IO Aliases
run as source =
  case runParser program "repl" source of
    Left e -> do
      putStrLn $ errorBundlePretty e
      return as
    Right ast -> do
      let (as', res) = evaluate as ast
      putStrLn . T.unpack $ prettyAST res
      return as'

repl :: Aliases -> IO ()
repl as = do
  putStr "> "
  hFlush stdout
  input <- getLine
  as' <- run as $ T.pack input
  repl as'

main :: IO ()
main = repl M.empty
