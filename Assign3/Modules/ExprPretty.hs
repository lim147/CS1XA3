
{-|
Module : ExprPretty
Description : contains the costomizing Show instance of expressions
Copyright : Meijing Li @2018
License : WTFPL  
Maintainer : lim147@mcmaster.ca
Stability : experimental
Portability : POSIX

 
This module uses the 'Expr' datatype. It contains Show instance 
and provides a nicely printing-out form to expressions.

-}

module ExprPretty where
import ExprType

-- | parenthese the items
parens:: String -> String
parens ss = "(" ++ ss ++ ")"



{- |  Provodes a pretty representation of our datatype
      matching the DSL provoded in DiffExpr
-}
instance Show a => Show (Expr a) where
	show (Mult e1 e2) = parens (show e1) ++ " * " ++ parens (show e2)
	show (Add e1 e2) = parens (show e1) ++ " + " ++ parens (show e2)
	show (Exp e c) = parens (show e) ++ " ^ " ++ show c
	show (Log c e) = "log " ++ show c ++ " " ++ parens (show e)
	show (Sin e) = "sin " ++ parens (show e)
	show (Cos e) = "cos " ++ parens (show e)
	show (Ln e) = "ln " ++ parens (show e)
	show (Var ss) = "var \"" ++ ss ++  "\""
	show (Const c) =  "val " ++ show c






