{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module : ExprParser
Description : contains the parsers to parse a formatted string into an expression datatype
Copyright : Meijing Li @2018
License : WTFPL  
Maintainer : lim147@mcmaster.ca
Stability : experimental
Portability : POSIX

 
This module uses the 'Expr' datatype. The parsers in this module take a string of 
format and parse it into an expression of corresponding type (i.e Int, Float, Double...)



-}





module ExprParser (parserExprD, parserExprF,parserExprI, parserExprIg,parserVectorExprD,parserVectorExprF,parserVectorExprI,parserVectorExprIg) where
--only allow users to access to the final function
--hide info from users

import ExprType
import ExprVector
import ExprDiff
import UniverNumType

import Text.Parsec
import Text.Parsec.String



{- A class to parse the values to the corresponding type.-}
class (DiffExpr a) => ParseValue a where
  value :: Parser a

instance ParseValue Double where
  value = double

instance ParseValue Float where
  value = float

instance ParseValue Int where
  value = int

instance ParseValue Integer where
  value = integer





{- Language Specification for Number:
 - ------------------------------------------------------------------------------------------------------------
 -           Type Encoding                         |     String Representation
 - ------------------------------------------------------------------------------------------------------------
 -           val   1.0                             |     1.0 
 -           var "x"                               |     x 
 -           (val -1) !*  var "x"                  |     -x
 -           val 1.0 !+ val 2.0                    |     1.0 + 2.0 
 -           val 1 !+ ((val -1) * (var "x"))       |     1 - x
 -           val 1.0 !* var "x"                    |     1.0 * x
 -           exp (var "x") 2                       |     x ^ 2
 -           loga 10 (var "x")                     |     log 10 x
 -           sine (val 1)                          |     sin 1
 -           cosi (var "x")                        |     cos x
 -           ln (var "x")                          |     ln x
 -           val 3.0 !* ((var "x") ^ -1.0)         |     3/x              
 -        
 - ------------------------------------------------------------------------------------------------------------
 -}


{- Language Specification for Vector:
 - ------------------------------------------------------------------------------------------------------------
 -           Type Encoding                         |     String Representation
 - ------------------------------------------------------------------------------------------------------------
 -           valV   [1,2,3,4]                       |     [1,2,3,4]
 -           valV   [-1,-2,-3,-4]                   |     -[1,2,3,4]
 -           varV "x"                               |     x 
 -           valV [1,2,3] ?+ valV [4,5,6]           |     [1,2,3] + [4,5,6]
 -           varV "x" ?+ valV [-1,-2,-3,-4]         |     x - [1,2,3,4]
 -           valV [1,2,3] ?* varV [4,5,6]           |     [1,2,3] * [4,5,6]
 -           valV [1,2,3] ?* varV "x"               |     [1,2,3] * x
 -           sineV (valV [1,2,3])                   |     sin [1,2,3]
 -           cosiV (varV "x")                       |     cos x
 -           lnV (varV "x")                         |     ln x 
 -           error                                  |     ... ^ ... 
 -           error                                  |     log ...
 -           error                                  |     ... / ...
 - ------------------------------------------------------------------------------------------------------------
 -}


-- ** Parse to Expr Double
parserExprD::String  -- ^ takes a string 
              -> Expr Double -- ^ parses it into a Expr of Double
parserExprD ss = case parse exprD "" ss of
	                Left err -> error $ "Parse Error: " ++ show err
	                Right expr -> expr


exprD ::Parser (Expr Double)
exprD = expr


{- Test cases:
	--------------------------------------------------------------------------------------
	- for exprD:
		- parse exprD "" "x + (y * 2)" == Right (var "x") + ((val 2.0) * (var "y"))

    - parse exprD "" "x + ((sin x) * (cos x))" == Right (var "x") + ((sin (var "x")) * (cos (var "x")))

	--------------------------------------------------------------------------------------
	- for parserExprD:
		- parserExprD "x + (y * 2)" == (var "x") + ((val 2.0) * (var "y"))
		- parserExprD "x + 0" == var "x"
		- parserExprD "x + 1 + y + 2 + z + 1" == (val 4.0) + (((var "x") + (var "y")) + (var "z"))
		- parserExprD "(x+1)^2" == ((val 1.0) + (var "x")) ^ 2.0
    - parserExprD "(x+1)/2" == (val 0.5) + ((val 0.5) * (var "x"))
		- parserExprD "1+((x+1)^2)" == (val 1.0) + (((val 1.0) + (var "x")) ^ 2.0)
    - parserExprD "(ln 10) + 2" == val 4.302585092994046
    - parserExprD "(log 10 (x + 1)) + (2 * 3)" == (val 6.0) + (log 10.0 ((val 1.0) + (var "x")))


-}




-- ** Parse to Expr Float
parserExprF::String  -- ^ takes a string 
              -> Expr Float -- ^ parses it into a Expr of Float
parserExprF ss = case parse exprF "" ss of
                  Left err -> error $ "Parse Error: " ++ show err
                  Right expr -> expr


exprF ::Parser (Expr Float)
exprF = expr


{- Test cases for Expr Float:
-----------------------------------------------------------------------------
	- for parserExprE:
		- parserExprF "x + 1 + y + 2 + z + 1" == (val 4.0) + (((var "x") + (var "y")) + (var "z"))

-}




-- ** Parse to Expr Int


-- | Note: 
--  As longs as meet dot, the parsing stops instead of returning an error, 
--  so please choose  correct parserExpr*      
parserExprI::String  -- ^ takes a string 
              -> Expr Int -- ^ parses it into a Expr of Int
parserExprI ss = case parse exprI "" ss of
                  Left err -> error $ "Parse Error: " ++ show err
                  Right expr -> expr


exprI ::Parser (Expr Int)
exprI = expr



-- ** Parse to Expr Integer

-- | Note: 
--   As longs as meet dot, the parse stops instead of return an eroor, 
--   so please choose  correct parserExpr*  
parserExprIg::String  -- ^ takes a string 
              -> Expr Integer -- ^ parses it into a Expr of Integer
parserExprIg ss = case parse exprIg "" ss of
                  Left err -> error $ "Parse Error: " ++ show err
                  Right expr -> expr


exprIg ::Parser (Expr Integer)
exprIg = expr

{-Test cases for Expr Integer:
-----------------------------------------------------------------------------------------------------------
	- for parserExprI:

		- parserExprI "x + 1 + y + 2 + z + 1" == (val 4) + (((var "x") + (var "y")) + (var "z"))

		- parserExprI "x + 1 + y + 2 + z + 1.0 + h" == (val 4) + (((var "x") + (var "y")) + (var "z"))

-}




{- ----------------------------------------------------------------------------------------
 - The Parser for the general type: Expr a
 - ----------------------------------------------------------------------------------------
-}


{-simple expression parser is for the expressions after sin, cos, ln, exp-}
--------------------------------------------------------------------------------
--factor, such as x , 1.0
factor ::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
factor = try negIdentifier <|> identifier <|> constant 


--simple term, like x, 1.0, (x+1.0)
simpleTerm :: (Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
simpleTerm = (parens simpleExpr) <|> factor

--simple expr, only contains +, * ops
simpleExpr :: (Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
simpleExpr = simpleTerm `chainl1` binaryOp
--------------------------------------------------------------------------------




-- complex entry, like x^2, sin x ...
entry ::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
entry = try expOp <|> try unaryOp <|> try divOp <|> try minusOp <|> try logOp <|>factor

entryD::Parser (Expr Double)
entryD = entry

--complex term , such as (sin x + 1.0), sin x, ln 10
term ::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
term =  entry <|> (parens expr) 

termD::Parser (Expr Double)
termD = term
 
-- takes a string and parses it into the Expr Double
expr ::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
expr = term `chainl1` binaryOp





binaryOp ::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a -> Expr a -> Expr a)
binaryOp  =     do {symbol "+" ; return (!+) }  --(!+) :: Expr a -> Expr a -> Expr a
            <|> do {symbol "*" ; return (!*) }  --(!*) :: Expr a -> Expr a -> Expr a


-- ln will both be parsed as ln (nature e)
unaryOp ::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
unaryOp =      do {symbol "sin" ; e <- simpleExpr ; return (sine e) }
          <|>  do {symbol "cos" ; e <- simpleExpr ; return (cosi e) }
          <|>  do {symbol "ln"  ; e <- simpleExpr ; return (ln e) }


expOp::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
expOp  = do e <- simpleExpr
            symbol "^"
            c <- parens value <|> value
            return (e !^ c )

logOp::(Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
logOp  = do symbol "log"
            c <- parens value <|> value
            spaces
            e <- simpleExpr
            return (loga c e)
           


-- parse "\" to exponent -1
divOp :: (Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
divOp  = do d1 <- simpleExpr
            symbol "/"
            d2 <- simpleExpr
            return (d1 !* (d2 !^ (-1)))

divOpD::Parser (Expr Double)
divOpD = divOp

minusOp :: (Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
minusOp  = do d1 <- simpleExpr
              symbol "-"
              d2 <- simpleExpr
              return (Add d1 (Mult (Const (-1)) d2)) -- d1 - d2 = d1 + (-1)*d2

constant :: (Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
constant  = do d <- value
               return (val d)

{-
  - letter: Parses a letter (an upper case or lower case character according to isAlpha). 
            Returns the parsed character.
-}
identifier :: (Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
identifier = do d <- many1 letter
                return (var d)



negIdentifier :: (Num a, DiffExpr a, ParseValue a) => Parser (Expr a)
negIdentifier  = do symbol "-"
                    d <- many1 letter
                    return (Mult (Const (-1)) (var d))

{- Test cases for Expr a:
   -----------------------------------------------------------------------------------
    - for constant:
      - parse constant "" "-12.3" == Right val -12

   ------------------------------------------------------------------------------------
     - for identifier:
      - parse identifier "" "xy" == Right var "xy"
      - parse identifier "" "x" == Right var "x"
  ------------------------------------------------------------------------------------
     - for negIdentifier:
      - parse negIdentifier "" "-y" == Right (val -1) * (var "y")
      - parse negIdentifier "" "-xy" == Right (val -1) * (var "xy")
  ------------------------------------------------------------------------------------
    - for divOp:
    divOpD::Parser (Expr Double)
    divOpD = divOp

      - parse divOpD "" "(x + 1)/2" == Right (val 0.5) + ((val 0.5) * (var "x"))
      - parse divOp "" "x/x" ==  Right val 1

  ------------------------------------------------------------------------------------
  
  - for expOp:
    - parse expOp "" "x ^ 2" == Right (var "x") ^ 2

    - parse expOp "" "(x+1)^2" == Right ((val 1) + (var "x")) ^ 2
  ------------------------------------------------------------------------------------
  
  - for logOp:
    - parse logOp "" "log 2 3" == Right val 2

    - parse logOpD "" "log 10 (x+1)" == Right log 10 ((val 1) + (var "x"))

  ------------------------------------------------------------------------------------
  - for unaryOp:
    - parse unaryOp "" "sin x" == Right sin (var "x")

    - parse unaryOp "" " cos x" == Right cos (var "x")

    - parse unaryOp "" " ln 3" == Right val val 1

    - parse unaryOp "" " ln x" == Right ln (var "x")

  --------------------------------------------------------------------------------------
  - for factor:
    - parse factor "" "x" == Right var "x"
    - parse factor "" "1" == Right val 1

  --------------------------------------------------------------------------------------
  - for entry:
    - parse entry "" "sin x" == Right sin (var "x")
    - parse entry "" "x" == Right var "x"
    - parse entry "" "x ^ 2" == Right (var "x") ^ 2

  --------------------------------------------------------------------------------------
  - for term:
  termD::Parser (Expr Double)
  termD = term

    - parse termD "" "(x + 1.0)"  == Right (val 1.0) + (var "x")
    - parse termD "" "sin (x + 1.0)" == Right sin ((val 1.0) + (var "x"))

-}





















-- ** Parse to Expr [Double]
parserVectorExprD::String  -- ^ takes a string 
              -> Expr [Double] -- ^ parses it into a Expr of [Double]
parserVectorExprD ss = case parse exprVD "" ss of
                  Left err -> error $ "Parse Error: " ++ show err
                  Right expr -> expr



{-simple expression parser is for the expressions after sin, cos, ln, exp-}
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--factor for Vector float, such as [1.0,2.0,3.5], x
factorVD::Parser (Expr [Double])
factorVD = identifierVD <|> constantVD


--simple term, like x, [1,2,3], (x+[1,2,3])
simpleTermVD:: Parser (Expr [Double])
simpleTermVD = (parens simpleExprVD) <|> factorVD

--simple expr, only contains +, * ops
simpleExprVD::Parser (Expr [Double])
simpleExprVD = simpleTermVD `chainl1` binaryOpVD

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------



-- complex entry, like : sin (x + [1,2,3]) ...
entryVD::Parser (Expr [Double])
entryVD = try expOpVD <|> try unaryOpVD <|> try divOpVD <|> try logOpVD <|>factorVD


--complex term , such as (sin x + [1,2]), sin x, ln [1,2]
termVD::Parser (Expr [Double])
termVD = try (parens exprVD) <|> entryVD 

-- takes a string and parses it into the Expr Double
exprVD::Parser (Expr [Double])
exprVD = termVD `chainl1` binaryOpVD





binaryOpVD:: Parser (Expr [Double] -> Expr [Double] -> Expr [Double])
binaryOpVD =     do {symbol "+" ; return (?+) }  --(!+) :: Expr a -> Expr a -> Expr a
             <|> do {symbol "*" ; return (?*) }  --(!*) :: Expr a -> Expr a -> Expr a


-- either ln or ln will both be parsed as ln (nature e)
unaryOpVD::Parser (Expr [Double])
unaryOpVD =     do {symbol "sin" ; e <- simpleExprVD ; return (sineV e) }
           <|>  do {symbol "cos" ; e <- simpleExprVD ; return (cosiV e) }
           <|>  do {symbol "ln" ; e <- simpleExprVD ; return (lnV e) }
          


expOpVD::Parser (Expr [Double])
expOpVD = do e <- simpleExprVD
             symbol "^"
             return (error "Exponentiation is not available in Vector")


logOpVD::Parser (Expr [Double])
logOpVD = do symbol "log"
             return (error "Logarithm is not available in Vector")


-- no division op in Vector
divOpVD::Parser (Expr [Double])
divOpVD = do d1 <- simpleExprVD
             symbol "/"
             return (error "Division is  not available in Vector")

constantVD::Parser (Expr [Double])
constantVD = do d <- vectorD 
                return (valV d)


identifierVD::Parser (Expr [Double])
identifierVD = do d <- many1 letter
                  return (varV d)







{- Test cases for parserVectorExprD:
  - parserVectorExprD "x + [1]" == (val [1.0]) + (var "x")

  - parserVectorExprD "(sin [1,2,3]) + [1,2,3]" == val [1.8414709848078965,2.909297426825682,3.1411200080598674]

  - parserVectorExprD "ln [1,2,3]" == val [0.0,0.6931471805599453,1.0986122886681098]

  - parserVectorExprD "(ln [1,2,3]) * sin [1,2,3]" == val [0.0,0.6302769476946344,0.15503617503151282]


-}






-- ** Parse to Expr [Float]
parserVectorExprF::String  -- ^ takes a string 
              -> Expr [Float] -- ^ parses it into a Expr of [Float]
parserVectorExprF ss = case parse exprVF "" ss of
                  Left err -> error $ "Parse Error: " ++ show err
                  Right expr -> expr




{-simple expression parser is for the expressions after sin, cos, ln, exp-}
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--factor for Vector float, such as [1.0,2.0,3.5], x
factorVF::Parser (Expr [Float])
factorVF = identifierVF <|> constantVF


--simple term, like x, [1,2,3], (x+[1,2,3])
simpleTermVF:: Parser (Expr [Float])
simpleTermVF = (parens simpleExprVF) <|> factorVF

--simple expr, only contains +, * ops
simpleExprVF::Parser (Expr [Float])
simpleExprVF = simpleTermVF `chainl1` binaryOpVF

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------



-- complex entry, like : sin (x + [1,2,3]) ...
entryVF::Parser (Expr [Float])
entryVF = try expOpVF <|> try unaryOpVF <|> try divOpVF <|> try logOpVF <|>factorVF


--complex term , such as (sin x + [1,2]), sin x, ln [1,2]
termVF::Parser (Expr [Float])
termVF = try (parens exprVF) <|> entryVF 

-- takes a string and parses it into the Expr Double
exprVF::Parser (Expr [Float])
exprVF = termVF `chainl1` binaryOpVF





binaryOpVF:: Parser (Expr [Float] -> Expr [Float] -> Expr [Float])
binaryOpVF =     do {symbol "+" ; return (?+) }  --(!+) :: Expr a -> Expr a -> Expr a
             <|> do {symbol "*" ; return (?*) }  --(!*) :: Expr a -> Expr a -> Expr a


-- either ln or ln will both be parsed as ln (nature e)
unaryOpVF::Parser (Expr [Float])
unaryOpVF =     do {symbol "sin" ; e <- simpleExprVF ; return (sineV e) }
           <|>  do {symbol "cos" ; e <- simpleExprVF ; return (cosiV e) }
           <|>  do {symbol "ln" ; e <- simpleExprVF ; return (lnV e) }
          


expOpVF::Parser (Expr [Float])
expOpVF = do e <- simpleExprVF
             symbol "^"
             return (error "Exponentiation is not available in Vector")


logOpVF::Parser (Expr [Float])
logOpVF = do symbol "log"
             return (error "Logarithm is not available in Vector")


-- no division op in Vector
divOpVF::Parser (Expr [Float])
divOpVF = do d1 <- simpleExprVF
             symbol "/"
             return (error "Division is  not available in Vector")

constantVF::Parser (Expr [Float])
constantVF = do d <- vectorF 
                return (valV d)


identifierVF::Parser (Expr [Float])
identifierVF = do d <- many1 letter
                  return (varV d)





-- ** Parse to Expr [Int]
parserVectorExprI::String  -- ^ takes a string 
              -> Expr [Int] -- ^ parses it into a Expr of [Int]
parserVectorExprI ss = case parse exprVI "" ss of
                  Left err -> error $ "Parse Error: " ++ show err
                  Right expr -> expr




{-simple expression parser is for the expressions after sin, cos, ln, exp-}
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--factor for Vector float, such as [1,2,3], x
factorVI::Parser (Expr [Int])
factorVI = identifierVI <|> constantVI


--simple term, like x, [1,2,3], (x+[1,2,3])
simpleTermVI:: Parser (Expr [Int])
simpleTermVI = (parens simpleExprVI) <|> factorVI

--simple expr, only contains +, * ops
simpleExprVI::Parser (Expr [Int])
simpleExprVI = simpleTermVI `chainl1` binaryOpVI

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------



-- complex entry, like : sin (x + [1,2,3]) ...
entryVI::Parser (Expr [Int])
entryVI = try expOpVI <|> try unaryOpVI <|> try divOpVI <|> try logOpVI <|>factorVI


--complex term , such as (sin x + [1,2]), sin x, ln [1,2]
termVI::Parser (Expr [Int])
termVI = try (parens exprVI) <|> entryVI 

-- takes a string and parses it into the Expr Double
exprVI::Parser (Expr [Int])
exprVI = termVI `chainl1` binaryOpVI





binaryOpVI:: Parser (Expr [Int] -> Expr [Int] -> Expr [Int])
binaryOpVI =     do {symbol "+" ; return (?+) }  --(!+) :: Expr a -> Expr a -> Expr a
             <|> do {symbol "*" ; return (?*) }  --(!*) :: Expr a -> Expr a -> Expr a


-- either ln or ln will both be parsed as ln (nature e)
unaryOpVI::Parser (Expr [Int])
unaryOpVI =     do {symbol "sin" ; e <- simpleExprVI ; return (sineV e) }
           <|>  do {symbol "cos" ; e <- simpleExprVI ; return (cosiV e) }
           <|>  do {symbol "ln" ; e <- simpleExprVI ; return (lnV e) }
           


expOpVI::Parser (Expr [Int])
expOpVI = do e <- simpleExprVI
             symbol "^"
             return (error "Exponentiation is not available in Vector")


logOpVI::Parser (Expr [Int])
logOpVI = do symbol "log"
             return (error "Logarithm is not available in Vector")


-- no division op in Vector
divOpVI::Parser (Expr [Int])
divOpVI = do d1 <- simpleExprVI
             symbol "/"
             return (error "Division is  not available in Vector")

constantVI::Parser (Expr [Int])
constantVI = do d <- vectorI 
                return (valV d)


identifierVI::Parser (Expr [Int])
identifierVI = do d <- many1 letter
                  return (varV d)






-- ** Parse to Expr [Integer]
parserVectorExprIg::String  -- ^ takes a string 
                -> Expr [Integer] -- ^ parses it into a Expr of [Integer]
parserVectorExprIg ss = case parse exprVIg "" ss of
                  Left err -> error $ "Parse Error: " ++ show err
                  Right expr -> expr




{-simple expression parser is for the expressions after sin, cos, ln, exp-}
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--factor for Vector float, such as [1,2,3], x
factorVIg::Parser (Expr [Integer])
factorVIg = identifierVIg <|> constantVIg


--simple term, like x, [1,2,3], (x+[1,2,3])
simpleTermVIg:: Parser (Expr [Integer])
simpleTermVIg = (parens simpleExprVIg) <|> factorVIg

--simple expr, only contains +, * ops
simpleExprVIg::Parser (Expr [Integer])
simpleExprVIg = simpleTermVIg `chainl1` binaryOpVIg

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------



-- complex entry, like : sin (x + [1,2,3]) ...
entryVIg::Parser (Expr [Integer])
entryVIg = try expOpVIg <|> try unaryOpVIg <|> try divOpVIg <|> try logOpVIg <|>factorVIg


--complex term , such as (sin x + [1,2]), sin x, ln [1,2]
termVIg::Parser (Expr [Integer])
termVIg = try (parens exprVIg) <|> entryVIg 

-- takes a string and parses it into the Expr Double
exprVIg::Parser (Expr [Integer])
exprVIg = termVIg `chainl1` binaryOpVIg





binaryOpVIg:: Parser (Expr [Integer] -> Expr [Integer] -> Expr [Integer])
binaryOpVIg =     do {symbol "+" ; return (?+) }  --(!+) :: Expr a -> Expr a -> Expr a
              <|> do {symbol "*" ; return (?*) }  --(!*) :: Expr a -> Expr a -> Expr a


-- either ln or ln will both be parsed as ln (nature e)
unaryOpVIg::Parser (Expr [Integer])
unaryOpVIg =     do {symbol "sin" ; e <- simpleExprVIg ; return (sineV e) }
            <|>  do {symbol "cos" ; e <- simpleExprVIg ; return (cosiV e) }
            <|>  do {symbol "ln" ; e <- simpleExprVIg ; return (lnV e) }
           


expOpVIg::Parser (Expr [Integer])
expOpVIg = do e <- simpleExprVIg
              symbol "^"
              return (error "Exponentiation is not available in Vector")



logOpVIg::Parser (Expr [Integer])
logOpVIg = do symbol "log"
              return (error "Logarithm is not available in Vector")



-- no division op in Vector
divOpVIg::Parser (Expr [Integer])
divOpVIg = do d1 <- simpleExprVIg
              symbol "/"
              return (error "Division is  not available in Vector")

constantVIg::Parser (Expr [Integer])
constantVIg = do d <- vectorIg 
                 return (valV d)


identifierVIg::Parser (Expr [Integer])
identifierVIg = do d <- many1 letter
                   return (varV d)





{- ------------------------------------------------------------------------------------------------------------
 - Utility Combinators
 - ------------------------------------------------------------------------------------------------------------
 -}

{-Part I: for Num -}
-----------------------------------------------------------
parens :: Parser a -> Parser a
parens p = do { char '(';
                cs <- p;
                char ')';
                return cs }

symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'


digits :: Parser String
digits = many1 digit


negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }


integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

int::Parser Int
int = fmap read $ try negDigits <|> digits



dotDigits::Parser String
dotDigits = do d1 <- digits
               symbol "."
               d2 <- digits
               return (d1++"."++d2)

negDotDigits::Parser String
negDotDigits = do symbol "-"
                  ds <- dotDigits
                  return ('-':ds)


double :: Parser Double
double =     (fmap read $ try negDotDigits <|> try dotDigits)  
         <|> (fmap read $ try negDigits <|> digits) 



float :: Parser Float
float =     (fmap read $ try negDotDigits <|> try dotDigits)  
        <|> (fmap read $ try negDigits <|> digits) 


{-Part II: for Vector -}
-------------------------------------------------------------------------
vectorD::Parser [Double]
vectorD = do symbol "["
             d1 <- (double `sepBy` (symbol ","))
             symbol "]"
             return d1 


vectorF::Parser [Float]
vectorF = do symbol "["
             d1 <- (float `sepBy` (symbol ","))
             symbol "]"
             return d1 



vectorI::Parser [Int]
vectorI = do symbol "["
             d1 <- (int `sepBy` (symbol ","))
             symbol "]"
             return d1 



vectorIg::Parser [Integer]
vectorIg = do symbol "["
              d1 <- (integer `sepBy` (symbol ","))
              symbol "]"
              return d1 



