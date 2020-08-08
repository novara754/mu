module Mu.Evaluator (Evaluator, Aliases, evaluate) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Mu.Parser

alphaConvert :: AST -> Identifier -> AST -> AST
alphaConvert (Variable n) v r
  | n == v    = r
  | otherwise = Variable n
alphaConvert (Application f a) v r =
  let f' = alphaConvert f v r
      a' = alphaConvert a v r
   in Application f' a'
alphaConvert (Abstraction n b) v r
  | n == v    = Abstraction n b
  | otherwise =
    let b' = alphaConvert b v r
     in Abstraction n b'

betaReduce :: AST -> AST
betaReduce (Variable v) = Variable v
betaReduce (Abstraction v b) = Abstraction v (betaReduce b)
betaReduce (Application f a) =
  case betaReduce f of
    Abstraction v b -> alphaConvert b v a
    x               -> Application x (betaReduce a)

-- | Recursively reduce the tree until no more changes can be made.
reduce :: AST -> AST
reduce ast =
  let ast' = betaReduce ast
   in if ast /= ast'
        then reduce ast'
        else ast

-- | Map to retrieve an expression by its alias.
type Aliases = M.Map Identifier AST

-- | Evaluator state for keeping track of known aliases.
type Evaluator = State Aliases AST

-- | Reduces an expression to normal form. Saves it in the list of known
--   aliases if an alias was given.
evaluate :: Aliased -> Evaluator
evaluate (Aliased n e) = do
  e' <- eval e
  modify $ M.insert n e'
  return e'
evaluate (Unaliased e) = eval e

-- | Internal function to evaluate (reduce to normal form) an expression
eval :: AST -> Evaluator
eval e = do
  as <- M.toList <$> get
  return . reduce $ foldl (uncurry . alphaConvert) e as
