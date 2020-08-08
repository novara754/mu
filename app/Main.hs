module Main (main) where

import System.IO
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List (intercalate)
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
    Right exprs -> do
      let (res, as') = runState (sequence $ map evaluate exprs) as
      putStrLn $ intercalate " ; " $ map (T.unpack . prettyAST) res
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
