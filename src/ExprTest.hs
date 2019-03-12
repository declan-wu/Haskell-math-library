{-|
Module      : ExprTest
Description : 
Copyright   : (c) Song Tao Wu @2018
Maintainer  : wus92@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}


module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType
import           UniType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

sampleExpr1 :: Expr Int
sampleExpr1 = (var "x") !+ (var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


sampleExpr1 :: Expr Double
sampleExpr1 = (var "x") !+ (var "y")

test1 :: Double -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0

sampleExpr2 :: Expr Double
sampleExpr2 = (var "x") !* (var "y")

test2 :: Double -> Bool
test2 x = eval (Map.fromList [("x",x),("y",x)]) sampleExpr2 == x^2
