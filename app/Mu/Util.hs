module Mu.Util (prettyAST) where

import qualified Data.Text as T
import Mu.Parser

prettyAST :: AST -> T.Text
prettyAST (Variable v)                        = v
prettyAST (Abstraction v b@(Application _ _)) = "λ" <> v <> ".(" <> prettyAST b <> ")"
prettyAST (Abstraction v b)                   = "λ" <> v <> "." <> prettyAST b
prettyAST (Application f a@(Application _ _)) = prettyAST f <> " (" <> prettyAST a <> ")"
prettyAST (Application f a)                   = prettyAST f <> " " <> prettyAST a
