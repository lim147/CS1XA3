{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module : ExprVector
Description : contains a type class and instances for
			  vector expressions.
Copyright : Meijing Li @2018
License : WTFPL  
Maintainer : lim147@mcmaster.ca
Stability : experimental
Portability : POSIX

 
This module uses the 'Expr' datatype, and `UniversalNum` class.
It contains a type class and instances for 
vector expressions, and can conduct evaluation, simplicication 
to those expressions.


Vector is represented as a LIST of numbers, and the types of numbers are Double, Float, Int, Integer respectively.


NOTE: Partial Differentiation , Exponentation  and arbitary based Logarithm are not available in the Vector Space.


-}

module ExprVector where

import  ExprType
import  ExprPretty
import  UniverNumType

import qualified Data.Map as Map


-- * Class VectorSpace

{- | This class has methods over the 'Expr' datatype
     that assist with construction and evaluation
     of vector expressions
-}

class (Eq a) => VectorSpace a where
	-- | takes a dictionary of variable identifiers and values, and uses it to compute the Expr fully
	evalV :: Map.Map String a -> Expr a -> a

	-- | takes a possible incomplete dictionary and uses it to reduce Expr as much as possible
	simpV :: Map.Map String a -> Expr a -> Expr a
	
	


	-- | simpVlify the expression recursive times untill it can't be reduced anymore
	simplifyV :: Map.Map String a -> Expr a -> Expr a
	simplifyV vrs  e 
	 | e == simpV vrs e = e
	 | otherwise = simplifyV vrs (simpV vrs e)

	-- | Binary Addition & simpVlification
	-- 
	-- >>> (valV [1,2,3.0] ?+ valV [1,2,3]) :: Expr [Float]
	-- val [2.0,4.0,6.0]
	(?+) :: Expr a -> Expr a -> Expr a
	e1 ?+ e2 = simplifyV (Map.fromList []) $ Add e1 e2 
	
	-- | Binary production & simplification
	-- 
	-- The Multiplication of two vectors of same length is a new vector with enties equal to the production of the entries from the 2 vectors in the corresponding position.
	(?*) :: Expr a -> Expr a -> Expr a
	e1 ?* e2 = simplifyV (Map.fromList []) $ Mult e1 e2

	-- | Exponentiation & simplification
	(?^) :: Expr a -> a -> Expr a 
	e ?^ c = simplifyV (Map.fromList []) $ Exp e c 

	-- | Logarithm of arbitory base & simplification
	logaV :: a -> Expr a -> Expr a 
	logaV c e = simplifyV (Map.fromList []) $ Log c e

	-- | Unary Sin op & simplification
	sineV :: Expr a -> Expr a 
	sineV e = simplifyV (Map.fromList []) $ Sin e

	-- | Unary Cos & simplification
	cosiV :: Expr a -> Expr a
	cosiV e = simplifyV (Map.fromList []) $ Cos e

	-- | Unary Ln & simplification
	lnV :: Expr a -> Expr a
	lnV e = simplifyV (Map.fromList []) $ Ln e
	
	-- | Wrapper for simple values
	valV :: a -> Expr a
	valV c = Const c
	
	-- | String identifier for variables
	varV::String -> Expr a
	varV s = simplifyV (Map.fromList []) $ Var s





-- Vector instance 
instance (UniversalNum a) => VectorSpace [a] where
	--evalV :: Map.Map String [a] -> Expr [a] -> [a]
	evalV vrs (Add e1 e2) 
	  | length (evalV vrs e1) == length (evalV vrs e2) = zipWith (\x y -> x + y) v1 v2
	  | otherwise = error "Vector not in same length. Addition is only available to same size vectors."
	       where v1 = (evalV vrs e1)
	             v2 = (evalV vrs e2)

	evalV vrs (Mult e1 e2) 
	  | length (evalV vrs e1) == length (evalV vrs e2) = zipWith (\x y -> x * y) v1 v2
	  | otherwise = error "Vector not in same length. Multiplication is only available to same size vectors."
	         where v1 = (evalV vrs e1)
	               v2 = (evalV vrs e2)

	evalV vrs (Exp e v2) = error "Exponentiation is not available in  Vector"

	evalV vrs (Log c e) = error "Arbitary based logarithm is not available in Vector"

	evalV vrs (Sin e) =  map (\x -> universalSin x) (evalV vrs e)

	evalV vrs (Cos e) =  map (\x -> universalCos x) (evalV vrs e)

	evalV vrs (Ln e) =   map (\x -> universalLn  x) (evalV vrs e)

	evalV vrs (Const v) = v
	evalV vrs (Var x) = case Map.lookup x vrs of
		                   Just v -> v
		                   Nothing -> error "failed lookup in evalV"


    --simpV :: Map.Map String [a] -> Expr [a] -> Expr [a]
    
	{-simpVlification for Addition: -}
	simpV vrs (Add (Const v1) (Const v2))  
		| length v1 == length v2 = Const $ zipWith (\x y -> x + y) v1 v2
		| otherwise = error "Vector not in same size."

	simpV vrs (Add (Var x) (Const v)) = Add (Const v) (simpV vrs (Var x)) -- x + v = v + x

	simpV vrs (Add (Const v1) (Add (Const v2) e)) 
		| length v1 == length v2 = Add (Const (zipWith (\x y -> x + y) v1 v2)) (simpV vrs e) -- v1+(v2+expr) = (v1+v2)+expr
		| otherwise = error "Vector not in same size."

	simpV vrs (Add (Add (Const v1) e) (Const v2)) 
		| length v1 == length v2 = Add (Const (zipWith (\x y -> x + y) v1 v2)) (simpV vrs e) --(v1+expr)+v2 = (v1+v2)+expr
		| otherwise = error "Vector not in same size."

	simpV vrs (Add (Add (Const v) e1) e2) = Add (Const v) (Add (simpV vrs e1) (simpV vrs e2)) --(v+expr1)+expr2 = v+(expr1+expr2)

	simpV vrs (Add e (Const v)) = Add (Const v) (simpV vrs e) -- expr + v = v + expr

	simpV vrs (Add (Var x) (Var y))  -- y + x = x + y
		| x <= y = Add (simpV vrs (Var x)) (simpV vrs(Var y))
		| otherwise = Add (simpV vrs(Var y)) (simpV vrs(Var x))



	{- simpVlification for mult: -}
	simpV vrs (Mult (Const v1) (Const v2)) 
	 | length v1 == length v2 = Const $ zipWith (\x y -> x * y) v1 v2 -- Vector multiplication
	 | otherwise = error "Vector not in same size."
	
	simpV vrs (Mult (Const v1) (Mult (Const v2) e)) 
	 | length v1 == length v2 = Mult (Const (zipWith (\x y -> x * y) v1 v2)) (simpV vrs e) -- a*(b*expr) = (a*b)*expr
	 | otherwise = error "Vector not in same size."

	simpV vrs (Mult (Const v1) (Mult e (Const v2))) 
	 | length v1 == length v2 = Mult (Const (zipWith (\x y -> x * y) v1 v2)) (simpV vrs e) -- a*(expr*b) = (a*b)*expr
	 | otherwise = error "Vector not in same size."

	simpV vrs (Mult (Mult (Const v1) e) (Const v2)) 
	 | length v1 == length v2 = Mult (Const (zipWith (\x y -> x * y) v1 v2)) (simpV vrs e) -- (a*expr)*b = (a*b)*expr
	 | otherwise = error "Vector not in same size."

	simpV vrs (Mult e (Const v)) = Mult (Const v) (simpV vrs e)  -- expr * a = a * expr

	simpV vrs (Mult e (Mult (Const v1) (Const v2))) 
	 | length v1 == length v2 = Mult (Const (zipWith (\x y -> x * y) v1 v2)) (simpV vrs e) -- expr*(a*b) = (a*b)*expr
	 | otherwise = error "Vector not in same size."







	{- simpVlication for Add & Mult combination: -}
	simpV vrs (Mult (Const v) (Add e1 e2)) = Add (Mult (Const v) (simpV vrs e1)) 
	                                            (Mult (Const v) (simpV vrs e2)) -- a * (e1 + e2) = a*e1+a*e2

	simpV vrs (Add (Var x) (Mult (Const v) (Var y)))  -- x + a*x = (a+1) * x
	    | x == y =  Mult (Const (map (\x -> x+1) v)) (simpV vrs (Var x))
	    | x < y =  Add (simpV vrs (Var x)) (Mult (Const v) (simpV vrs (Var y)))
	    | otherwise = Add (Mult (Const v) (simpV vrs (Var y))) (simpV vrs (Var x))
                                      

	simpV vrs (Add (Mult (Const v) (Var x)) (Var y)) -- a*x + x = (a+1) * x
	    | x == y = Mult (Const (map (\x -> x+1) v)) (simpV vrs (Var x))
	    | x < y =  Add (Mult (Const v) (simpV vrs (Var x))) (simpV vrs (Var y))
	    | otherwise = Add (simpV vrs (Var y)) (Mult (Const v) (simpV vrs (Var x)))
                                       
                                       

	simpV vrs (Add (Mult (Const v1) (Var x)) (Mult (Const v2) (Var y))) -- a*x+b*x = (a+b)*x
	    | x == y = Mult (Const (zipWith (\x y -> x + y) v1 v2)) (simpV vrs (Var x))
	    | x < y = Add (Mult (Const v1) (simpV vrs (Var x))) (Mult (Const v2) (simpV vrs (Var y)))
	    | otherwise = Add (Mult (Const v2) (simpV vrs (Var y))) (Mult (Const v1) (simpV vrs (Var x)))
	



	{-simpVlification for Exp:-}
	simpV vrs (Exp e v) = error "Exponentiation is not available in Vector"


	{-simpVlification for Log:-}
	simpV vrs (Log c _) = error "Logarithm is not available in Vector"

	{-simpVlification for Sin:-}
	simpV vrs (Sin (Const v)) = Const $ map (\x -> universalSin x) v
	simpV vrs (Sin e) = Sin (simpV vrs e)
	

	{-simpVlification for Cos:-}
	simpV vrs (Cos (Const v)) = Const $ map (\x -> universalCos x) v
	simpV vrs (Cos e) = Cos (simpV vrs e)


	{-simpVlification for ln:-}
	simpV vrs (Ln (Const v)) = Const $ map (\x -> universalLn x) v
	simpV vrs (Ln e) = Ln (simpV vrs e)


 

	
	simpV vrs (Add e1 e2) = Add (simpV vrs e1) (simpV vrs e2)

	

	simpV vrs (Mult e1 e2) = Mult (simpV vrs e1) (simpV vrs e2)



	simpV vrs (Const v) = Const v
	simpV vrs (Var x) = case Map.lookup x vrs of
		                   Just v -> Const v
		                   Nothing -> Var x
	

	


	





{- Test cases:
	- for Vector:
----------------------------------------------------------------------------------------------
	- valV [1,2,3] ?+ valV [1,2,3] == val [2,4,6]

	- sineV (valV [1,2,3]) == val [1,1,0]

	- lnV (valV [1.0,2.0,3]) = val [0.0,0.6931471805599453,1.0986122886681098]

	- varV "x" ?* sineV (valV [1,2,3]) == (val [1,1,0]) * (var "x")

	- simplifyV (Map.fromList [("x",[1,2,3])]) $ (varV "x" ?+ valV [1,2,3])== val [2,4,6]

	- simplifyV (Map.fromList [("x",[1,2])]) $ (varV "x" ?+ valV [1,2,3]) == error: Vector not in same space

-}
	











