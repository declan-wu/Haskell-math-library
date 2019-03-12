{-|
Module      : ExprType
Description : Contains Numerical Expressions Datatype
Copyright   : (c) Song Tao Wu @2018
Maintainer  : wus92@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprType where

import           Data.List


-- * Section: Datatype Declaration
-- | a datatype encoding numeric expressions
   
data Expr a = Add (Expr a) (Expr a) -- ^ Binary addition
            | Mult (Expr a) (Expr a)-- ^ Binary multiplication
            | Div (Expr a) (Expr a) -- ^ Div - binary division
            | Cos (Expr a)          -- ^ Cosine function
            | Sin (Expr a)          -- ^ Sine function
            | Log a (Expr a)        -- ^ Log of base a           
            | Ln (Expr a)           -- ^ Natural logarithm
                | E (Expr a)            -- ^ Natural exponentiation e^(Expr a) 
            | Pow (Expr a) (Expr a) -- ^ Exponentiate a function
            | Const a               -- ^ Wrap a constant value
            | Var String            -- ^ Wrap a variable identifier
  deriving Eq


-- | getVars retrives variable identifiers from an expression.
 
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 ++ getVars e2
getVars (Mult e1 e2) = getVars e1 ++ getVars e2
getVars (Div e1 e2)  = getVars e1 ++ getVars e2
getVars (Cos e1)     = getVars e1
getVars (Sin e1)     = getVars e1
getVars (Log b e1)   = getVars e1
getVars (Ln e1)      = getVars e1
getVars (E e1)       = getVars e1
getVars (Pow e1 e2)  = (getVars e1) ++ (getVars e2)
getVars (Const _)    = []
getVars (Var x)      = [x]