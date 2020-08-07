module Mu.Evaluator (Evaluator, Aliases, evaluate, reduce, alphaConversion, betaReduction) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Mu.Parser

alphaConversion :: AST -> Identifier -> AST -> AST
alphaConversion (Variable n) v r
  | n == v    = r
  | otherwise = Variable n
alphaConversion (Application f a) v r =
  let f' = alphaConversion f v r
      a' = alphaConversion a v r
   in Application f' a'
alphaConversion (Abstraction n b) v r
  | n == v    = Abstraction n b
  | otherwise =
    let b' = alphaConversion b v r
     in Abstraction n b'
alphaConversion (Alias _ _) _ _ = error "unreachable"

betaReduction :: AST -> AST
betaReduction (Variable v) = Variable v
betaReduction (Abstraction v b) = Abstraction v (betaReduction b)
betaReduction (Application f a) =
  case betaReduction f of
    Abstraction v b -> alphaConversion b v a
    x               -> Application x (betaReduction a)
betaReduction (Alias _ _) = error "unreachable"

-- | Recursively reduce the tree until no more changes can be made.
reduce :: AST -> AST
reduce ast =
  let ast' = betaReduction ast
   in if ast /= ast'
        then reduce ast'
        else ast

type Aliases = M.Map Identifier AST

type Evaluator = State Aliases AST

evaluate :: AST -> Evaluator
evaluate (Alias n e) = do
  e' <- evaluate e
  modify $ M.insert n e'
  return e'
evaluate e = do
  as <- M.toList <$> get
  return $ foldl (uncurry . alphaConversion) e as

-- evaluate (Alias n e) =
--   let (_, e')  = evaluate as e
--       as' = M.insert n e' as
--    in (as', e')
-- evaluate as e =
--   let e' = foldl (\ast (alias, value) -> alphaConversion ast alias value) e (M.toList as)
--    in (as, reduce e')
