module Main (main) where

import System.IO

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  putStrLn input
  repl

main :: IO ()
main = repl
