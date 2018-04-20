{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module : UniverNumType
Description : contains a type class and instances for
			  universal operations.
Copyright : Meijing Li @2018
License : WTFPL  
Maintainer : lim147@mcmaster.ca
Stability : experimental
Portability : POSIX


The module works with 'Expr' datatype.

The class in the module makes several calculation methods work with all Num type.
It expands the domain of functions sin, cos ,ln , log, exponent, 1/ln  
to both Floating and Integral types. It's a subclass of 'DiffExpr' and 'VectorSpace' classes
to support them work with all Num type.



The instances contain Double, Float, Int, Integer 4 basic numerical types.

-}



module UniverNumType where

import ExprType


import qualified Data.Map as Map



-- * Class UniverNumType: Univerversal Numerical Type

{- | This class has universal methods for calcuating sin, cos, natural log, log, exponent, and 1/(ln a)
     over both Floating and Integral numbers.
-}



class (Num a, Eq a) => UniversalNum a where
	universalSin :: a -> a 
	universalCos :: a -> a
	universalLn  :: a -> a
	universalExp :: a -> a -> a
	universalLog :: a -> a -> a
	-- | the coefficient part of the derivatice of log a e ,i.e.  1/ln a
	universalLogDiffCoe :: a -> a 

instance UniversalNum Double where
	universalSin x = sin x
	universalCos x = cos x
	universalLn  x = log x
	universalExp x c = x ** c
	universalLog c x = logBase c x
	universalLogDiffCoe x = 1 / log x 

instance UniversalNum Float where
	universalSin x = sin x
	universalCos x = cos x
	universalLn  x = log x
	universalExp x c = x ** c
	universalLog c x = logBase c x
	universalLogDiffCoe x = 1 / log x

instance  UniversalNum Int where
	universalSin x = round $ sin (fromIntegral x)
	universalCos x = round $ cos (fromIntegral x) 
	universalLn  x = round $ log (fromIntegral x) 
	universalExp x c = x ^ c
	universalLog c x = round $ logBase (fromIntegral c) (fromIntegral x)
	universalLogDiffCoe x = round $ 1/log (fromIntegral x)

	
instance  UniversalNum Integer where
	universalSin x = round $ sin (fromIntegral x)
	universalCos x = round $ cos (fromIntegral x) 
	universalLn  x = round $ log (fromIntegral x) 
	universalExp x c = x ^ c
	universalLog c x = round $ logBase (fromIntegral c) (fromIntegral x)
	universalLogDiffCoe x = round $ 1/log (fromIntegral x)




