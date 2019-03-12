
Assignment 3: Haskell Math Library
=======
Description: This is a project for course -- CS1XA3 

## ExprType
1. Contains Numerical Expressions Datatype
2. `getVars` retrives variable identifiers from an expression
3. The data type Exp a has constructors to encode:
 * Add (Expr a) (Expr a) -- ^ Binary addition
 * Mult (Expr a) (Expr a)-- ^ Binary multiplication
 * Div (Expr a) (Expr a) -- ^ Div - binary division
 * Cos (Expr a)          -- ^ Cosine function
 * Sin (Expr a)          -- ^ Sine function
 * Log a (Expr a)        -- ^ Log of base a           
 * Ln (Expr a)           -- ^ Natural logarithm
 * E (Expr a)            -- ^ Natural exponentiation e^(Expr a) 
 * Pow (Expr a) (Expr a) -- ^ Exponentiate a function
 * Const a               -- ^ Wrap a constant value
 * Var String            -- ^ Wrap a variable identifier

## ExprDiff
1. Contains a type class with following useful methods:
	* `eval`: Takes a dictionary of variable identifiers and values and an expression of Expr type, then evaluate it to some value.
	* `simplify`: Takes a possibly incomplete dictionary of variable identifiers and values and an expression of Expr type, then simplify it to as close to as normal form as possible.
	* `partDiff`: Takes a string (variable to be partially derived) and an expression of Expr type, then differentiates in terms of that (parially or fully).


## ExprUnitype
This class has methods, namely: numDiv, numLog, numLn, numCos, numSin and numPow to get DiffExpr to work with Floating and Integral numbers.

## ExprPretty
Provides a pretty representation of our datatype

## ExprParser
Contains functions that parse strings into an Expr type

## Reference & Credits
1. UniType ideas was from Allen Chen's [ShoeHornFloating class](https://github.com/zhouh46/CS1XA3/tree/master/Assign3)
2. Substantial parts of my parser file are shamless copy of [Jessica's ExprParser.hs] (https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3), which is permitted under her [liscence](https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3/LICENSE.md)
