{-|
Module      : ExprPretty
Description : Provides a pretty representation of our datatype
Copyright   : (c) Song Tao Wu @2018
Maintainer  : wus92@mcmaster.ca
Stability   : experimental
-}


module ExprPretty where

import           ExprType


parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Add e1 e2)  = parens $ (show e1) ++ " !+ " ++ parens (show e2)
  show (Mult e1 e2) = parens $ (show e1) ++ " !* " ++ parens (show e2)
  show (Div e1 e2)  = parens $ (show e1) ++ " !/ " ++ parens (show e2)
  show (Cos e1)     = parens $ "Cosine " ++ (show e1)
  show (Sin e1)     = parens $ "Sine " ++ (show e1)
  show (Ln e1)      = parens $ "Ln " ++ (show e1)
  show (Log a e)    = parens $ "log' base " ++ (show a) ++ " " ++ (show e)
  show (E e1)       = parens $ "e ^ " ++ (show e1)
  show (Pow e1 e2)  = parens $ (show e1) ++ "!^" ++ (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x