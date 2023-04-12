module Typing where

import Syntax

conv :: Type -> Type -> Bool
conv Unit Unit = True
conv (Function a b) (Function a' b') =
  conv a a' && conv b b'
conv _ _ = False

-- Inference takes a context and a raw term and infers if the term is
-- well-typed neutral. Fails with Nothing otherwise
-- Includes elaboration
--  Γ ⊢ t => A
infer :: Context -> Raw -> Maybe (Term, Type)
infer gamma (RVar x) = do
  (ix, ty) <- inferVar gamma
  pure (Var ix, ty)
  where
    inferVar :: Context -> Maybe (Ix, Type)
    -- Variable not in scope
    inferVar [] = Nothing
    inferVar (gamma :> (x', t))
      | x == x' = pure (0, t)
      | otherwise = do
          (ix, t) <- inferVar gamma
          pure (ix + 1, t)
infer gamma (RApp t u) = do
  (t, Function a b) <- infer gamma t
  u <- check gamma u a
  pure (App t u, b)
infer gamma (RLet x a t u) = do
  t <- check gamma t a
  (u, uty) <- infer (gamma :> (x, a)) u
  pure (Let x a t u, uty)
-- Inference fails on non-neutral forms
infer _ _ = Nothing

-- Check takes a context, a raw term and a type and succeeds if well typed.
-- Includes elaboration
--  Γ ⊢ t <= A
check :: Context -> Raw -> Type -> Maybe Term
check _ ROne Unit = pure One
check gamma (RLam x t) (Function a b) = do
  t <- check (gamma :> (x, a)) t b
  pure (Lam x t)
check gamma (RLet x a t u) b = do
  t <- check gamma t a
  u <- check (gamma :> (x, a)) u b
  pure (Let x a t u)
check gamma t a = do
  (t, a') <- infer gamma t
  if conv a a'
    then pure t
    else -- Conversion failed (inferred type different from checked type)
      Nothing
