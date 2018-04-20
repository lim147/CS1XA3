{-|
Module : ExprTest
Description : contains the test cases and properties
Copyright : Meijing Li @2018
License : WTFPL  
Maintainer : lim147@mcmaster.ca
Stability : experimental
Portability : POSIX

 
This module uses the 'Expr' datatype. 

It contains sample expressions and several properties for testing functions defined in 'DiffExpr' and
'VectorSpace' datatype.

To use QuickCheck, you could simply just type `quickCheck property_fun_name` in the editor, e.g.

>>> quickCheck testD1
+++ OK, passed 100 tests.


-}

module ExprTest where

import ExprType
import ExprDiff
import ExprVector
import UniverNumType

import ExprParser
import ExprPretty

import qualified Data.Map as Map
import Test.QuickCheck


-- * Test Properties & Cases for Differentiable Expr

-- ** 1. Test for Expr Int

-- | a sample expression over Int 
--
-- x + y
sampleExprI1::Expr Int
sampleExprI1 = var "x" !+ var "y"

-- | a sample expression over Int 
--
-- x * y
sampleExprI2::Expr Int
sampleExprI2 = var "x" !* var "x"



-- | generate a list of Int for quickcheck over `Expr Int`
listToExprI::[Int] -> Expr Int
listToExprI [a] = Const a
listToExprI (a:as) = Add (Const a) (listToExprI as)
listToExprI [] = error "can't list to expression for empty"


-- | Check if the expression satisfies:
-- 
-- prop>  a + (-a) = 0
testI1 :: Int -> Bool
testI1 a = eval (Map.fromList [("x",a),("y",-a)]) sampleExprI1 == 0


-- | Check if `eval` works well on adding multiple constants together
testI2::[Int] -> Bool
testI2 xs 
 | xs == [] = True
 | otherwise = eval (Map.fromList []) (listToExprI xs) == sum xs


-- | Check if `eval` satisfies:
--
-- prop> expr * 0 = 0
testI3::[Int] -> Bool
testI3 xs 
 | xs == [] = True
 | otherwise = eval (Map.fromList []) (Mult (listToExprI xs) (Const 0)) == 0


-- | Check if `simplify` could add up all the contants together
testI4::[Int] -> Bool
testI4 xs 
 | xs == [] = True
 | otherwise = simplify (Map.fromList []) (listToExprI xs) == val (sum xs)


-- | Check if `simplify` could reduce an expression after timing 0 to 0
testI5::[Int] -> Bool
testI5 xs
 | xs == [] = True
 | otherwise = simplify (Map.fromList []) (Mult (listToExprI xs) (Const 0)) == val 0



-- | check the property: 
--
-- prop> d(x ^ 2 + constant)/dx  = d(x ^ 2)/ dx
testI6::Int -> Bool
testI6 c = partDiff "x" (sampleExprI2 !+ val c) == partDiff "x" sampleExprI2


-- | check the property: partial derivative of a non-existing variable of an expr is 0
--
-- prop> d(f(x,y))/ dz = 0
testI7 ::Int -> Bool
testI7 c = partDiff "z" (sampleExprI1 !+ val c) == val 0


-- | check the property: 
-- 
-- prop> (a ^ b) * (a ^ c) = a ^ (b+c)
testI8 :: Int -> Int -> Int -> Bool
testI8 a b c 
 | b < 0 || c < 0 = True
 | otherwise =  eval (Map.fromList [("x",a),("y",a)]) ((var "x" !^ b) !* (var "y" !^ c))  == a ^ (b+c)








-- ** 2. Test for Expr Integer

-- | a sample expression over Integer
--
-- x + y
sampleExprIg1::Expr Integer
sampleExprIg1 = var "x" !+ var "y"

-- | a sample expression over Integer
--
-- x * y
sampleExprIg2::Expr Integer
sampleExprIg2 = var "x" !* var "x"



-- | generate a list of Integer for quickcheck over `Expr Int`
listToExprIg::[Integer] -> Expr Integer
listToExprIg [a] = Const a
listToExprIg (a:as) = Add (Const a) (listToExprIg as)
listToExprIg [] = error "can't list to expression for empty"


-- | Check if the expression satisfies:
-- 
-- prop>  a + (-a) = 0
testIg1 :: Integer -> Bool
testIg1 a = eval (Map.fromList [("x",a),("y",-a)]) sampleExprIg1 == 0


-- | Check if `eval` works well on adding multiple constants together
testIg2::[Integer] -> Bool
testIg2 xs 
 | xs == [] = True
 | otherwise = eval (Map.fromList []) (listToExprIg xs) == sum xs


-- | Check if `eval` satisfies:
--
-- prop> expr * 0 = 0
testIg3::[Integer] -> Bool
testIg3 xs 
 | xs == [] = True
 | otherwise = eval (Map.fromList []) (Mult (listToExprIg xs) (Const 0)) == 0


-- | Check if `simplify` could add up all the contants together
testIg4::[Integer] -> Bool
testIg4 xs 
 | xs == [] = True
 | otherwise = simplify (Map.fromList []) (listToExprIg xs) == val (sum xs)


-- | Check if  `simplify` could reduce an expression after timing 0 to 0
testIg5::[Integer] -> Bool
testIg5 xs
 | xs == [] = True
 | otherwise = simplify (Map.fromList []) (Mult (listToExprIg xs) (Const 0)) == val 0



-- | check the property: 
--
-- prop> d(x ^ 2 + constant)/dx  = d(x ^ 2)/ dx
testIg6::Integer -> Bool
testIg6 c = partDiff "x" (sampleExprIg2 !+ val c) == partDiff "x" sampleExprIg2


-- | check the property: partial derivitave of a non-existing variable of an expr is 0
--
-- prop> d(f(x,y))/ dz = 0
testIg7 ::Integer -> Bool
testIg7 c = partDiff "z" (sampleExprIg1 !+ val c) == val 0


-- | check the property: 
-- 
-- prop> (a ^ b) * (a ^ c) = a ^ (b+c)
testIg8 :: Integer -> Integer -> Integer -> Bool
testIg8 a b c 
 | b < 0 || c < 0 = True
 | otherwise =  eval (Map.fromList [("x",a),("y",a)]) ((var "x" !^ b) !* (var "y" !^ c))  == a ^ (b+c)






-- ** 3. Test for Expr Double

sampleExprD1::Expr Double
sampleExprD1 = var "x" !+ var "y"

sampleExprD2::Expr Double
sampleExprD2 = var "x" !* var "x"


-- | generate a list of Double for quickcheck over (Expr Double)
listToExprD::[Double] -> Expr Double
listToExprD [a] = Const a
listToExprD (a:as) = Add (Const a) (listToExprD as)
listToExprD [] = error "can't list to expression for empty"


-- | Check if the expression satisfies:
-- 
-- prop>  a + (-a) = 0
testD1 :: Double -> Bool
testD1 a = abs (eval (Map.fromList [("x",a),("y",-a)]) sampleExprD1) <= tol
            where tol = 0.005


-- | Check if `eval` workes well on adding multiple constants together
testD2::[Double] -> Bool
testD2 xs 
 | xs == [] = True
 | otherwise = abs ((eval (Map.fromList []) (listToExprD xs)) - sum xs) <= tol
       where tol = 0.005

-- | Check if `eval` satisfies:
--
-- prop> expr * 0 = 0
testD3::[Double] -> Bool
testD3 xs 
 | xs == [] = True
 | otherwise = eval (Map.fromList []) (Mult (listToExprD xs) (Const 0)) == 0


-- | Check if `simplify` could add up all the contants together
testD4::[Double] -> Bool
testD4 xs 
 | xs == [] = True
 | otherwise = let 
      tol = 0.005
     in case simplify (Map.fromList []) (listToExprD xs) of
 	               Const s -> (s - sum xs) < tol
 	               _       -> False

      


-- | Check if `simplify` could reduce an expression after timing 0 to 0
testD5::[Double] -> Bool
testD5 xs
 | xs == [] = True
 | otherwise = simplify (Map.fromList []) (Mult (listToExprD xs) (Const 0)) == val 0



-- | check the property: 
--
-- prop> d(x ^ 2 + constant)/dx  = d(x ^ 2)/ dx
testD6::Double -> Bool
testD6 c = partDiff "x" (sampleExprD2 !+ val c) == partDiff "x" sampleExprD2


-- | check the property: partial derivitave of a non-existing variable of an expr is 0
--
-- prop> d(f(x,y))/ dz = 0
testD7 ::Double -> Bool
testD7 c = partDiff "z" (sampleExprD1 !+ val c) == val 0


-- | check the property: 
-- 
-- prop> (a ^ b) * (a ^ c) = a ^ (b+c)
testD8 :: Double -> Double -> Double -> Bool
testD8 a b c   
 | a <= 0 = True -- a < 0 may cause NAN error
 | (abs a > 10 || abs b > 10 || abs c > 10) = True -- limit the numbers in some domain, easy to control the tol
 | otherwise = abs (evalValue - value) < tol
     where evalValue = eval (Map.fromList [("x",a),("y",a)]) ((var "x" !^ b) !* (var "y" !^ c))
           value = a ** (b+c)
           tol = 0.05 




-- ** 4. Test for Expr Float

sampleExprF1::Expr Float
sampleExprF1 = var "x" !+ var "y"

sampleExprF2::Expr Float
sampleExprF2 = var "x" !* var "x"


-- | generate a list of Double for quickcheck over (Expr Double)
listToExprF::[Float] -> Expr Float
listToExprF [a] = Const a
listToExprF (a:as) = Add (Const a) (listToExprF as)
listToExprF [] = error "can't list to expression for empty"


-- | Check if the expression satisfies:
-- 
-- prop>  a + (-a) = 0
testF1 :: Float -> Bool
testF1 a = abs (eval (Map.fromList [("x",a),("y",-a)]) sampleExprF1) <= tol
            where tol = 0.005


-- | Check if `eval`workes well on adding multiple constants together
testF2::[Float] -> Bool
testF2 xs 
 | xs == [] = True
 | otherwise = abs ((eval (Map.fromList []) (listToExprF xs)) - sum xs) <= tol
       where tol = 0.005

-- | Check if `eval` satisfies:
--
-- prop> expr * 0 = 0
testF3::[Float] -> Bool
testF3 xs 
 | xs == [] = True
 | otherwise = eval (Map.fromList []) (Mult (listToExprF xs) (Const 0)) == 0


-- | Check if `simplify` could add up all the contants together
testF4::[Float] -> Bool
testF4 xs 
 | xs == [] = True
 | otherwise = let 
      tol = 0.005
     in case simplify (Map.fromList []) (listToExprF xs) of
 	               Const s -> (s - sum xs) < tol
 	               _       -> False

      

    
 

-- | Check if `simplify` could reduce an expression after timing 0 to 0
testF5::[Double] -> Bool
testF5 xs
 | xs == [] = True
 | otherwise = simplify (Map.fromList []) (Mult (listToExprD xs) (Const 0)) == val 0



-- | check the property: 
--
-- prop> d(x ^ 2 + constant)/dx  = d(x ^ 2)/ dx
testF6::Float -> Bool
testF6 c = partDiff "x" (sampleExprF2 !+ val c) == partDiff "x" sampleExprF2


-- | check the property: partial derivitave of a non-existing variable of an expr is 0
--
-- prop> d(f(x,y))/ dz = 0
testF7 ::Float -> Bool
testF7 c = partDiff "z" (sampleExprF1 !+ val c) == val 0


-- | check the property: 
-- 
-- prop> (a ^ b) * (a ^ c) = a ^ (b+c)
testF8 :: Float -> Float -> Float -> Bool
testF8 a b c   
 | a <= 0 = True -- a < 0 may cause NAN error
 | (abs a > 10 || abs b > 10 || abs c > 10) = True -- limit the numbers in some domain, easy to control the tol
 | otherwise = abs (evalValue - value) < tol
     where evalValue = eval (Map.fromList [("x",a),("y",a)]) ((var "x" !^ b) !* (var "y" !^ c))
           value = a ** (b+c)
           tol = 0.05 










-- * Test Properties & Cases for Vector Expr


-- ** 1. Test for Expr [Int]

-- | a sample vector expression over [Int]
--
-- x + y
sampleVectorExprI1::Expr [Int]
sampleVectorExprI1 = varV "x" ?+ varV "y"

-- | a sample vector expression over [Int]
--
-- x * y
sampleVectorExprI2::Expr [Int]
sampleVectorExprI2 = varV "x" ?* varV "y"



-- | Check if the expression satisfies:
-- 
-- prop>  v + (-v) = [0,0,0]
testVI1 :: Int -> Int -> Int -> Bool
testVI1 a b c = evalV (Map.fromList [("x",[a,b,c]),("y",[-a,-b,-c])]) sampleVectorExprI1 == [0,0,0]


-- | Check if `simplifyV` satisfies:
--
-- prop> v * 0 = 0
testVI2::Int -> Int -> Int -> Bool
testVI2 a b c = simplifyV (Map.fromList [("x",[a,b,c]),("y",[0,0,0])]) sampleVectorExprI2 == valV [0,0,0]








-- ** 2. Test for Expr [Integer]

-- | a sample vector expression over [Intger]
--
-- x + y
sampleVectorExprIg1::Expr [Integer]
sampleVectorExprIg1 = varV "x" ?+ varV "y"

-- | a sample vector expression over [Integer]
--
-- x * y
sampleVectorExprIg2::Expr [Integer]
sampleVectorExprIg2 = varV "x" ?* varV "y"





-- | Check if the expression satisfies:
-- 
-- prop>  v + (-v) = [0,0,0]
testVIg1 :: Integer -> Integer -> Integer -> Bool
testVIg1 a b c = evalV (Map.fromList [("x",[a,b,c]),("y",[-a,-b,-c])]) sampleVectorExprIg1 == [0,0,0]


-- | Check if `simplifyV` satisfies:
--
-- prop> v * 0 = 0
testVIg2::Integer -> Integer -> Integer -> Bool
testVIg2 a b c = simplifyV (Map.fromList [("x",[a,b,c]),("y",[0,0,0])]) sampleVectorExprIg2 == valV [0,0,0]








-- ** 3. Test for Expr [Double]

-- | a sample vector expression over [Double]
--
-- x + y
sampleVectorExprD1::Expr [Double]
sampleVectorExprD1 = varV "x" ?+ varV "y"

-- | a sample vector expression over [Double]
--
-- x * y
sampleVectorExprD2::Expr [Double]
sampleVectorExprD2 = varV "x" ?* varV "y"



-- | Check if the expression satisfies:
-- 
-- prop>  v + (-v) = [0,0,0]
testVD1 :: Double -> Double -> Double -> Bool
testVD1 a b c = let 
      value = evalV (Map.fromList [("x",[a,b,c]),("y",[-a,-b,-c])]) sampleVectorExprD1 
      tol = 0.005
	in case value of
	       [t1,t2,t3] -> (abs t1 < tol) && (abs t2 < tol) && (abs t3 < tol)
	       _          -> False
                                             

-- | Check if `simplifyV` satisfies:
--
-- prop> v * 0 = 0
testVD2::Double -> Double -> Double -> Bool
testVD2 a b c = simplifyV (Map.fromList [("x",[a,b,c]),("y",[0,0,0])]) sampleVectorExprD2 == valV [0,0,0]








-- ** 4. Test for Expr [Float]

-- | a sample vector expression over [Float]
--
-- x + y
sampleVectorExprF1::Expr [Float]
sampleVectorExprF1 = varV "x" ?+ varV "y"

-- | a sample vector expression over [Float]
--
-- x * y
sampleVectorExprF2::Expr [Float]
sampleVectorExprF2 = varV "x" ?* varV "y"



-- | Check if the expression satisfies:
-- 
-- prop>  v + (-v) = [0,0,0]
testVF1 :: Float -> Float -> Float-> Bool
testVF1 a b c = let 
      value = evalV (Map.fromList [("x",[a,b,c]),("y",[-a,-b,-c])]) sampleVectorExprF1 
      tol = 0.005
	in case value of
	       [t1,t2,t3] -> (abs t1 < tol) && (abs t2 < tol) && (abs t3 < tol)
	       _          -> False
                                             

-- | Check if `simplifyV` satisfies:
--
-- prop> v * 0 = 0
testVF2::Float -> Float -> Float -> Bool
testVF2 a b c = simplifyV (Map.fromList [("x",[a,b,c]),("y",[0,0,0])]) sampleVectorExprF2 == valV [0,0,0]





