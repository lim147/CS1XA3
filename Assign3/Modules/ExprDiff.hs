{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module : ExprDiff
Description : contains a type class and instances for
			  differentiable expressions
Copyright : Meijing Li @2018
License : WTFPL  
Maintainer : lim147@mcmaster.ca
Stability : experimental
Portability : POSIX

 
This module uses the 'Expr' datatype, and `UniversalNum` class.
It contains a type class and instances for 
differentiable expressions, and can conduct evaluation, simplicication,
and partial differentiation to the  differentiable expressions.



The instances contain Double, Float, Int, Integer 4 basic numerical types.
   


-}



module ExprDiff where

import  ExprType
import  ExprPretty
import  UniverNumType 

import qualified Data.Map as Map



-- * Class DiffExpr: Differentiable Expression

{- | This class has methods over the 'Expr' datatype
     that assist with construction and evaluation
     of differentiable expressions
-}

class (Eq a) => DiffExpr a where
	-- | takes a dictionary of variable identifiers and values, and uses it to compute the Expr fully
	eval :: Map.Map String a -> Expr a -> a

	-- | takes a possible incomplete dictionary and uses it to reduce Expr as much as possible
	simp :: Map.Map String a -> Expr a -> Expr a
	
	-- | given a var identifier, differentiate IN TERMS of that identifier
	partDiff :: String -> Expr a -> Expr a
	


	-- | simplify the expression recursively untill it can't be reduced anymore
	-- 
	-- >>> simplify (Map.fromList [("x",2)]) $ (val 2 !+ var "x") !+ (val 4 !+ var "y")
	-- (val 8) + (var "y")

	simplify :: Map.Map String a -> Expr a -> Expr a
	simplify vrs  e 
	 | e == simp vrs e = e
	 | otherwise = simplify vrs (simp vrs e)

	-- | Binary Addition & simplification
	(!+) :: Expr a -> Expr a -> Expr a
	e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2 
	
	-- | Binary Miltiplication & simplification
	(!*) :: Expr a -> Expr a -> Expr a
	e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2

	-- | Exponentiation & simplification
	(!^) :: Expr a -> a -> Expr a 
	e !^ c = simplify (Map.fromList []) $ Exp e c 

	-- | Logarithm of arbitory base & simplification
	loga :: a -> Expr a -> Expr a 
	loga c e = simplify (Map.fromList []) $ Log c e

	-- | Unary Sin op & simplification
	sine :: Expr a -> Expr a 
	sine e = simplify (Map.fromList []) $ Sin e

	-- | Unary Cos & simplification
	cosi :: Expr a -> Expr a
	cosi e = simplify (Map.fromList []) $ Cos e

	-- | Unary Ln & simplification
	ln :: Expr a -> Expr a
	ln e = simplify (Map.fromList []) $ Ln e
	
	-- | Wrapper for simple values
	val :: a -> Expr a
	val c = Const c
	
	-- | String identifier for variables
	var::String -> Expr a
	var s = simplify (Map.fromList []) $ Var s

	




--  Num instances of DiffExpr
--  Most intuative instances 

instance (UniversalNum a, Eq a) => DiffExpr a where 
	eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
	eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2 
	eval vrs (Exp e c) = universalExp (eval vrs e) c
	eval vrs (Log c e) = universalLog c (eval vrs e)
	eval vrs (Sin e) = universalSin (eval vrs e)
	eval vrs (Cos e) = universalCos (eval vrs e)
	eval vrs (Ln e) =  universalLn (eval vrs e)
	eval vrs (Const x) = x
	eval vrs (Var s) = case Map.lookup s vrs of
		                   Just v -> v
		                   Nothing -> error "failed lookup in eval"

	

	{- Simplification for add: -}
	simp vrs (Add (Const 0) e) = simp vrs e -- 0 + expr = expr
	simp vrs (Add e (Const 0)) = simp vrs e -- expr + 0 = expr
	simp vrs (Add (Const a) (Const b)) = Const (a+b) -- a + b = a + b
	simp vrs (Add (Var x) (Const a)) = Add (Const a) (simp vrs (Var x)) -- x + a = a + x
	simp vrs (Add (Const a) (Add (Const b) e)) = Add (Const (a+b)) (simp vrs e) -- a+(b+expr) = (a+b)+expr
	simp vrs (Add (Add (Const a) e) (Const b)) = Add (Const (a+b)) (simp vrs e) --(a+expr)+b = (a+b)+expr
	simp vrs (Add (Add (Const a) e1) e2) = Add (Const a) (Add (simp vrs e1) (simp vrs e2)) --(a+expr1)+expr2 = a+(expr1+expr2)
	simp vrs (Add e (Const a)) = Add (Const a) (simp vrs e) -- expr + a = a + expr
	simp vrs (Add (Var x) (Var y))  -- y + x = x + y
		| x < y = Add (simp vrs (Var x)) (simp vrs(Var y))
		| x == y = Mult (Const 2) (simp vrs(Var x))
		| otherwise = Add (simp vrs(Var y)) (simp vrs(Var x))



	{- Simplification for mult: -}
	simp vrs (Mult (Const 0) e) = Const 0  -- x * expr
	simp vrs (Mult e (Const 0)) = Const 0  -- 0 * expr = 0
	simp vrs (Mult (Const a) (Const b)) = Const (a*b) -- a * b = (a*b)
	simp vrs (Mult e (Const 1)) = simp vrs e -- e * 1 = e
	simp vrs (Mult (Const 1) e) = simp vrs e -- 1 * e = e
	simp vrs (Mult (Const a) (Mult (Const b) e)) = Mult (Const (a*b)) (simp vrs e) -- a*(b*expr) = (a*b)*expr
	simp vrs (Mult (Const a) (Mult e (Const b))) = Mult (Const (a*b)) (simp vrs e) -- a*(expr*b) = (a*b)*expr
	simp vrs (Mult (Mult (Const a) e) (Const b)) = Mult (Const (a*b)) (simp vrs e) -- (a*expr)*b = (a*b)*expr
	simp vrs (Mult e (Const a)) = Mult (Const a) (simp vrs e)  -- expr * a = a * expr
	simp vrs (Mult e (Mult (Const a) (Const b))) = Mult (Const (a*b)) (simp vrs e) -- expr*(a*b) = (a*b)*expr


    {- Simplication for Add & Mult combination: -}
	simp vrs (Mult (Const a) (Add e1 e2)) = Add (Mult (Const a) (simp vrs e1)) 
	                                                (Mult (Const a) (simp vrs e2)) -- a * (e1 + e2) = a*e1+a*e2

	simp vrs (Add (Var x) (Mult (Const a) (Var y)))  -- x + a*x = (a+1) * x
	    | x == y =  Mult (Const (a+1)) (simp vrs (Var x))
	    | x < y =  Add (simp vrs (Var x)) (Mult (Const a) (simp vrs (Var y)))
	    | otherwise = Add (Mult (Const a) (simp vrs (Var y))) (simp vrs (Var x))
                                      

	simp vrs (Add (Mult (Const a) (Var x)) (Var y)) -- a*x + x = (a+1) * x
	    | x == y = Mult (Const (a+1)) (simp vrs (Var x)) 
	    | x < y =  Add (Mult (Const a) (simp vrs (Var x))) (simp vrs (Var y))
	    | otherwise = Add (simp vrs (Var y)) (Mult (Const a) (simp vrs (Var x)))
                                       
                                       

	simp vrs (Add (Mult (Const a) (Var x)) (Mult (Const b) (Var y))) -- a*x+b*x = (a+b)*x
	    | x ==y = Mult (Const (a+b)) (simp vrs (Var x))
	    | x < y = Add (Mult (Const a) (simp vrs (Var x))) (Mult (Const b) (simp vrs (Var y)))
	    | otherwise = Add (Mult (Const b) (simp vrs (Var y))) (Mult (Const a) (simp vrs (Var x)))
	




    {-Simplification for Exponent:-}
	simp vrs (Exp (Const a) c) = Const $ universalExp a c  -- a ^ c = a ^ c

	simp vrs (Mult (Exp e1 a) (Exp e2 b))
	 | simp vrs e1 == simp vrs e2 = Exp e1 (a+b)  -- e^a * e^b = e^(a+b)
	 | otherwise = Mult (Exp (simp vrs e1) a) (Exp (simp vrs e2) b)

	simp vrs (Mult e1 (Exp e2 (-1)))  -- e * e ^ -1 = 1
	 | simp vrs e1 == simp vrs e2 = Const 1
	 | otherwise = Mult (simp vrs e1) (Exp (simp vrs e2) (-1))

	simp vrs (Exp e c) 
	 | c == 0 = Const 1
	 | otherwise = Exp (simp vrs e) c
 



	{-Simplification for Log:-}
	simp vrs (Log c (Const a)) = Const $ universalLog c a
	simp vrs (Log c (Mult e1 e2)) = Add (Log c (simp vrs e1)) (Log c (simp vrs e2)) --log e1*e2 = log e1 + log e2
	simp vrs (Log c (Exp e a)) = Mult (Const a) (Log c e) -- log (e ^ a) = a log e
	simp vrs (Log c e) = Log c (simp vrs e)


	{-Simplification for Sin:-}
	simp vrs (Sin (Const c)) = Const $ universalSin c
	simp vrs (Sin e) = Sin (simp vrs e)


	{-Simplification for Cos:-}
	simp vrs (Cos (Const c)) = Const $ universalCos c
	simp vrs (Cos e) = Cos (simp vrs e)



	{-Simplification for ln:-}
	simp vrs (Ln (Const c)) = Const $ universalLn c
	simp vrs (Ln e) = Ln (simp vrs e)



	simp vrs (Add e1 e2)    -- e + e = 2 * e 
	 | simp vrs e1 == simp vrs e2 = Mult (Const 2) e1
	 | otherwise = Add (simp vrs e1) (simp vrs e2)

	

	simp vrs (Mult e1 e2) = Mult (simp vrs e1) (simp vrs e2)



	simp vrs (Const a) = Const a
	simp vrs (Var x) = case Map.lookup x vrs of
		                   Just v -> Const v
		                   Nothing -> Var x




	partDiff x (Add e1 e2) = partDiff x e1 !+ partDiff x e2
	partDiff x (Mult e1 e2) = ((partDiff x e1) !* e2) !+ (e1 !* (partDiff x e2)) 
	partDiff x (Exp e a) = (Const a) !* (Exp e (a-1)) !* (partDiff x e)
	partDiff x (Log c e) = (Const (universalLogDiffCoe c))!* (Exp e (-1)) !* (partDiff x e)  -- (log c e)' = (1/lnc) * (1/e) * e'
	partDiff x (Sin e) = Cos e !* partDiff x e
	partDiff x (Cos e) = (Const (-1)) !* (Sin e) !* (partDiff x e)
	partDiff x (Ln e) = (Exp e (-1)) !* (partDiff x e)
	partDiff x (Const _) = Const 0
	partDiff x (Var y)  
		| x == y = Const 1
		| otherwise = Const 0
                               





  








