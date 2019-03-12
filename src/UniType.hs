{-|
Module      : UniType
Description : Contains a type class used by ExprDiff
Copyright   : (c) Song Tao Wu @2018
Maintainer  : wus92@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}


module UniType where

import ExprType
import qualified Data.Map as Map



-- * Class UniType: Univerversal Num Type

{- | This class has methods, namely: numDiv, numLog, numLn, numCos, numSin and numPow
     to get DiffExpr to work with Floating and Integral numbers.
-}

class (Num a, Eq a, Show a, Ord a) => UniNums a where
  numDiv :: a -> a -> a
  numE :: a -> a
  numLog :: a -> a -> a
  numLn :: a -> a
  numCos :: a -> a
  numSin :: a -> a
  numPow :: a -> a -> a

instance UniNums Float where
  numDiv a b = a / b
  numE x     = exp x
  numLog b x = logBase b x
  numLn x    = log x
  numCos x   = cos x
  numSin x   = sin x
  numPow b x = b ** x

instance UniNums Double where
  numDiv a b = a / b
  numE x     = exp x
  numLog b x = logBase b x
  numLn x    = log x
  numCos x   = cos x
  numSin x   = sin x
  numPow b x = b ** x

instance UniNums Int where
  numDiv a b = round $ (fromIntegral a) / (fromIntegral b)
  numE x     = round $ exp (fromIntegral x)
  numLog b x = round $ logBase (fromIntegral b) (fromIntegral x)
  numLn x    = round $ log (fromIntegral x)
  numCos x   = round $ cos (fromIntegral x)
  numSin x   = round $ sin (fromIntegral x)
  numPow b x = round $ (fromIntegral b) ** (fromIntegral x)

instance UniNums Integer where
  numDiv a b = round $ (fromInteger a) / (fromInteger b)
  numE x     = round $ exp (fromInteger x)
  numLog b x = round $ logBase (fromInteger b) (fromInteger x)
  numLn x    = round $ log (fromInteger x)
  numCos x   = round $ cos (fromInteger x)
  numSin x   = round $ sin (fromInteger x)
  numPow b x = round $ (fromInteger b) ** (fromInteger x)
