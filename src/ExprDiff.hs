{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : ExprDiff
Description : Contains functions that evaluate, differentiate and partial differentiate expressions
Copyright   : (c) Song Tao Wu @2018
Maintainer  : wus92@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}


module ExprDiff where

import ExprType
import ExprPretty
import UniType
import qualified Data.Map.Strict as Map



{-| Class DiffExpr
    This class contains following methods over Expr datatype:
    ------------------------------------------------------------------------------------------------------------------------
    eval:     Takes a dictionary of variable identifiers and values and an expression of Expr type, then evaluate it to some value
    simplify: Takes a possibly incomplete dictionary of variable identifiers and values and an expression of Expr type, then simplify it to as close to as normal form as possible
    partDiff: Takes a string (variable to be partially derived) and an expression of Expr type, then differentiates in terms of that (parially or fully)
    ------------------------------------------------------------------------------------------------------------------------
-}

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a

  {- Default methods applied over Expr type as common operators and then return the new value wrapped as Expr types -}
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  (!/) :: (UniNums a) => Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2  
  (!^) :: Expr a -> Expr a -> Expr a
  b !^ x = simplify (Map.fromList []) (Exponent b x)
  e :: (UniNums a) => Expr a -> Expr a
  e e = simplify (Map.fromList []) $ E e
  log' :: (UniNums a) => a -> Expr a -> Expr a
  log' a e = simplify (Map.fromList []) $ Log a e
  ln :: (UniNums a) => Expr a -> Expr a
  ln e = simplify (Map.fromList []) $ Ln e
  sine :: (UniNums a) => Expr a -> Expr a
  sine e = simplify (Map.fromList []) $ Sin e
  cosine :: (UniNums a) => Expr a -> Expr a
  cosine e = simplify (Map.fromList []) $ Cos e
  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x



instance (Floating a, Eq a) => DiffExpr a where

  -- | Evaluation of an expression
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  eval vrs (E e) = numE (eval vrs e)
  eval vrs (Pow b x) = numPow (eval vrs b) ** (eval vrs x)
  eval vrs (Cos e) = numCos (eval vrs e)
  eval vrs (Sin e) = numSin (eval vrs e)
  eval vrs (Ln e) = numLog (eval vrs e)
  eval vrs (Log a e)  = numLog a (eval vrs e)
  eval vrs (Ln e) = numLn (eval vrs e)


  -- | Simplification of an expression

  -- General simplification
  simplify vrs (Const a) = Const a
  simplify vrs (Var x) = case Map.lookup x vrs of
                           Just v -> Const (eval vrs (Var x))
                           Nothing -> Var x 
  
  simplify vrs (Mult (Const 0) _ ) = Const 0
  simplify vrs (Mult _ (Const 0)) = Const 0
  simplify vrs (Mult (Const 1) e1 ) = simplify vrs e1
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Ln (Const 1)) = Const 0
  simplify vrs (E (Const 0)) = Const 1
  simplify vrs (Pow _ (Const 0)) = Const 1 

  -- Simplification for addition
  simplify vrs (Add (Const 0) e) = simplify vrs e -- 0 + expr = expr
  simplify vrs (Add e (Const 0)) = simplify vrs e -- expr + 0 = expr
  simplify vrs (Add (Const a) (Const b)) = Const (a+b) -- a + b = a + b
  simplify vrs (Add (Var x) (Const a)) = Add (Const a) (simplify vrs (Var x)) -- x + a = a + x
  simplify vrs (Add (Const a) (Add (Const b) e)) = Add (Const (a+b)) (simplify vrs e) -- a+(b+expr) = (a+b)+expr
  simplify vrs (Add (Add (Const a) e) (Const b)) = Add (Const (a+b)) (simplify vrs e) --(a+expr)+b = (a+b)+expr
  simplify vrs (Add (Add (Const a) e1) e2) = Add (Const a) (Add (simplify vrs e1) (simplify vrs e2)) --(a+expr1)+expr2 = a+(expr1+expr2)
  simplify vrs (Add e (Const a)) = Add (Const a) (simplify vrs e) -- expr + a = a + expr
  simplify vrs (Add (Var x) (Var y))  -- y + x = x + y
    | x < y = Add (simplify vrs (Var x)) (simplify vrs(Var y))
    | x == y = Mult (Const 2) (simplify vrs(Var x))
    | otherwise = Add (simplify vrs(Var y)) (simplify vrs(Var x))



  -- Simplification for multiplication
  simplify vrs (Mult (Const 0) e) = Const 0  -- x * expr
  simplify vrs (Mult e (Const 0)) = Const 0  -- 0 * expr = 0
  simplify vrs (Mult (Const a) (Const b)) = Const (a*b) -- a * b = (a*b)
  simplify vrs (Mult e (Const 1)) = simplify vrs e -- e * 1 = e
  simplify vrs (Mult (Const 1) e) = simplify vrs e -- 1 * e = e
  simplify vrs (Mult (Const a) (Mult (Const b) e)) = Mult (Const (a*b)) (simplify vrs e) -- a*(b*expr) = (a*b)*expr
  simplify vrs (Mult (Const a) (Mult e (Const b))) = Mult (Const (a*b)) (simplify vrs e) -- a*(expr*b) = (a*b)*expr
  simplify vrs (Mult (Mult (Const a) e) (Const b)) = Mult (Const (a*b)) (simplify vrs e) -- (a*expr)*b = (a*b)*expr
  simplify vrs (Mult e (Const a)) = Mult (Const a) (simplify vrs e)  -- expr * a = a * expr
  simplify vrs (Mult e (Mult (Const a) (Const b))) = Mult (Const (a*b)) (simplify vrs e) -- expr*(a*b) = (a*b)*expr

  --Simplification of division
  simplify vrs (Div e1 e2) = let
  s1 = simplify vrs e1
  s2 = simplify vrs e2
  in case (s1, s2) of
      (e, Const 1)               -> s1 -- just itself
      (Const 0, _)               -> Const 0 -- 0 divided by anything is 0
      (Const a, Const b)         -> eval vrs (Div (Const a) (Const b))
      (Const a, Div (Const b) e) -> simplify vrs $ Div (Mult (Const a) e) (Const b) 
      (Var x, Var y)             -> if x == y
                                    then Const 1
                                    else Div s1 s2
      (Var x, e)                 -> Div (Var x) s2
      (e, Var x)                 -> Div s1 (Var x)
      (Pow e1 e2, Pow x1 x2)     -> if e1 == x1
                                    then simplify vrs $ Pow e1 (Sub e2 x2)
                                    else Div s1 s2
      (x1, x2)                   -> if x1 == x2
                                    then Const 1
                                    else Div x1 x2 



  --Simplification for Exponentiation
  simplify vrs (Exp (Const a) c) = Const $ uniExp a c  -- a ^ c = a ^ c

  simplify vrs (Mult (Exp e1 a) (Exp e2 b))
   | simplify vrs e1 == simplify vrs e2 = Exp e1 (a+b)  -- e^a * e^b = e^(a+b)
   | otherwise = Mult (Exp (simplify vrs e1) a) (Exp (simplify vrs e2) b)

  simplify vrs (Mult e1 (Exp e2 (-1)))  -- e * e ^ -1 = 1
   | simplify vrs e1 == simplify vrs e2 = Const 1
   | otherwise = Mult (simplify vrs e1) (Exp (simplify vrs e2) (-1))

  simplify vrs (Exp e c) 
   | c == 0 = Const 1
   | otherwise = Exp (simplify vrs e) c
 

  --Simplification for Log
  simplify vrs (Log c (Const a)) = Const $ uniLog c a
  simplify vrs (Log c (Mult e1 e2)) = Add (Log c (simplify vrs e1)) (Log c (simplify vrs e2)) --log e1*e2 = log e1 + log e2
  simplify vrs (Log c (Exp e a)) = Mult (Const a) (Log c e) -- log (e ^ a) = a log e
  simplify vrs (Log c e) = Log c (simplify vrs e)


  --Simplification for Sin and Cos
  simplify vrs (Sin (Const c)) = Const $ uniSin c
  simplify vrs (Sin e) = Sin (simplify vrs e)

  simplify vrs (Cos (Const c)) = Const $ uniCos c
  simplify vrs (Cos e) = Cos (simplify vrs e)



  --Simplification for ln
  simplify vrs (Ln (Const c)) = Const $ uniLn c
  simplify vrs (Ln e) = Ln (simplify vrs e)


  --Simplification of variables and constants
  simplify vrs (Const a) = Const a
  simplify vrs (Var x) = case Map.lookup x vrs of
                       Just v -> Const v
                       Nothing -> Var x


  -- | Partial differentiation of an expression
  -- Rules Used:
  -- Addition differentiation:
  --d(e1 + e2)/dx = d(e1)/dx + d(e2)/dx

  -- Product rule:
  --d(e1 * e2)/dx = d(e1)/dx * e2 + e1 * d(e2)/dx

  -- Quotient rule:
  --d(e1 / e2) /dx = (d(e1)/dx * e2 - e1 * d(e2)/dx) / (e2 ** 2)

  -- Power rule:
  --d(e ^ a)/dx = a * (e1) ^(a-1) * d(e1)/dx

  -- ln and log:
  --d(ln e)/dx = e ^ (-1) * d(e)/dx
  --d(log a e)/dx = (1/ln a) * 1/e * d(e)/dx

  -- Trig differentiation:
  --d(sin e)/dx = (cos e) * d(e)/dx
  --d(cos e)/dx = (-1) * (sin e) * d(e)/dx 

  -- Differentiation of a constant:
  --d(a)/dx = 0

  -- Differentiation of a variable:
  --d(x)/dx = 1

  -- Differentiation of a non-relevant variable:
  --d(y)/dx = 0

  partDiff v (Add e1 e2)  = Add (partDiff v e1) (partDiff v e2)
  partDiff v (Mult e1 e2) = let
    pd1 = partDiff v e1
    pd2 = partDiff v e2
    in Add (Mult pd1 e2) (Mult e1 pd2)

  partDiff v (Div (Const 1) e) = Mult (Mult (Const (-1)) (partDiff v e)) (Div (Const 1) (Pow e (Const 2)))
  partDiff v (Div e1 e2)       = let
    pd1 = partDiff v e1
    pd2 = partDiff v e2
    top = Sub (Mult pd1 e2) (Mult e1 pd2)
    bottom = Pow (e2) (Const 2)
    in (Div top bottom)

  partDiff v (E e)        = Mult (E e) (partDiff v e)
  partDiff v (Pow e1 e2)  = partDiff v (E (Mult e2 (Ln e1)))
  partDiff v (Log a e)    = Mult (Div (Const 1) (Ln (Const a))) (partDiff v e)
  partDiff v (Ln e)       = Mult (partDiff v e) (Div (Const 1) e)
  partDiff v (Cos e)      = Mult (partDiff v e) (Mult (Const (-1)) (Sin e))
  partDiff v (Sin e)      = Mult (partDiff v e) (Cos e)
  partDiff _ (Const a)    = Const 0
  partDiff v (Var b)      = if b == v then (Const 1) else (Const 0)

  
  

