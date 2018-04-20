{-|
Module : ExprType
Description : contains the expression datatype
Copyright : Meijing Li @2018
License : WTFPL  
Maintainer : lim147@mcmaster.ca
Stability : experimental
Portability : POSIX

 
This module contains the 'Expr' datatype, which wraps different operations 
in a Expression Tree.
It also contains a funtion to retrive variable from the expression.

-}

module ExprType where

import Data.List 

-- * Dataype Declaration

-- | a data type for Numerical expression 
data Expr a =  Add (Expr a) (Expr a)        -- ^ Binary Addition
			       | Mult (Expr a) (Expr a)       -- ^ Binary Miltiplication
             | Exp (Expr a) a               -- ^ Exponent 
             | Log a (Expr a)               -- ^ Logarithm of arbitory numerical base 
             | Sin (Expr a)                 -- ^ Unary Sin
             | Cos (Expr a)                 -- ^ Unary Cos
             | Ln (Expr a)                  -- ^ Unary natural logarithm
			       | Const a                      -- ^ Wrapper for simple values
			       | Var String                   -- ^ String identifier for variables
          
   deriving Eq





-- * Miscellaneous Functions

-- | a function to retrive variable ientifiers from an Expr
getVars::Expr a -> [String]
getVars (Add e1 e2) = getVars e1 `union` getVars e2     -- union: set union, same as ++, don't allow duplicated variables
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Exp e _) = getVars e 
getVars (Log _ e) = getVars e
getVars (Sin e) = getVars e
getVars (Cos e) = getVars e
getVars (Ln e) = getVars e
getVars (Const _) = []
getVars (Var ident) = [ident]





{-Test cases:
  
 - -------------------------------------------------------------------------------------
 - for getVars:

   - getVars (var "x" !+ var "y" !+ val 10) == ["x","y"]
   
   - getVars (sine (var "x") !* (var "y") !^ 2) == ["x","y"]

   - getVars (val 2 !^ 2 !+ cosi (var "z")) == ["z"]




-}




