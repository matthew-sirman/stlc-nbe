{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Syntax where

-- Variable names
type Name = String

-- Types
data Type
  = Unit
  | Function Type Type
  deriving (Show)

-- Raw syntactic terms
data Raw
  = -- x
    RVar Name
  | -- 1
    ROne
  | -- λx. t
    RLam Name Raw
  | -- t u
    RApp Raw Raw
  | -- let x : T = t in u
    RLet Name Type Raw Raw
  deriving (Show)

-- De Bruijn indices
newtype Ix = Ix Int
  deriving (Eq, Ord, Num, Show)

-- Elaborated term
data Term
  = Var Ix
  | One
  | Lam Name Term
  | App Term Term
  | Let Name Type Term Term
  deriving (Show)

-- Contexts are lists of variable names and their types
type Context = [(Name, Type)]

-- Flipped cons operator (snoc) so context extensions look natural:
--  Γ :> (x, τ)
infixl 4 :>
pattern (:>) :: [a] -> a -> [a]
pattern xs :> x = x : xs

-- Just tells GHC that (:>) and [] cover all cases for lists
{-# COMPLETE (:>), [] #-}

-- De Bruijn levels
newtype Lvl = Lvl Int
  deriving (Eq, Ord, Num)

data Val
  = VVar Lvl
  | VOne
  | VLam Name (Val -> Val)
  | VApp Val Val

-- Evaluation environment
type Env = [Val]
