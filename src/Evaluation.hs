module Evaluation where

import Syntax

-- Evaluate a syntactic term to a semantic value
eval :: Env -> Term -> Val
-- De Bruijn level variables are just lookups into the enviroment
eval env (Var (Ix x)) = env !! x
eval _ One = VOne
-- Lambdas become closures (Haskell functions which extend the environment with
-- the value they are called with)
eval env (Lam x t) = VLam x (\vx -> eval (env :> vx) t)
eval env (App t u) =
  let vu = eval env u
   in case eval env t of
        -- If the term evaluates to a lambda function then evaluate it
        VLam _ t -> t vu
        -- Otherwise, this is a stuck value
        vt -> VApp vt vu
eval env (Let _ _ t u) =
  -- Evaluate the body and extend the environment with the new value
  let vt = eval env t
   in eval (env :> vt) u

-- Quote turns a semantic value into a syntactic one (for printing)
-- The level is the "depth" at which we are quoting
quote :: Lvl -> Val -> Term
-- Converts a de Bruijn level value to a de Bruijn index
quote (Lvl l) (VVar (Lvl x)) = Var (Ix (l - x - 1))
quote _ VOne = One
quote lvl (VLam x vt) =
  let
    -- Create a fresh variable for a generic x at the current de Bruijn
    -- level
    vx = VVar lvl
    -- Apply the semantic function the fresh variable
    t_x = vt vx
   in
    -- Quote the resulting applied term at the next level (we have stepped one place deeper)
    Lam x (quote (lvl + 1) t_x)
quote lvl (VApp t u) = App (quote lvl t) (quote lvl u)

-- Normalisation by evaluation is achieved by quoting after evaluating.
-- This gives a β-normal form.
nbe :: Term -> Term
nbe = quote 0 . eval []
