 # Documentation for Assign 3 


## Overview
This assignment builds a math library in Haskell, which is composed of 6 modules providing different functionality for calculation:

- ExprType  -- contains an expression data type that can encode addition, multiplication, exponentation, logarithm, trigonometry (sin, cos), variables and constants.

- ExprDiff  -- contains a type class and instances for differentiable expression, and could conduct evaluation, simplification, partial differentation over these expressions.

- ExprVector -- contains a type class and instances for vector expressions, and could conduct certain evaluation, simplification over these expressions.

- ExprParser -- contains the parsers to parse a formatted string into an expression. (The format will provided later)

- ExprPretty -- contains the customer Show instance of expressions

- ExprTest -- contains the test cases and quickCheck methods of the above 5 module.


## ExprType

The module contains a data type `Expe a` for Numerical expression and a function `getVars` to retrive variable identifiers from an expression.

The data type `Exp a` has constructors to encode:
1. binary addition and multiplication 
2. exponentation, with expression as base and number as exponent, e.g. (x+1)^2 could be expressed as:
```sh
Exp (Add (Var "x") (Const 1)) 2 
```
3. logarithm, to some numerical base of an expression, e.g. log2 (x+1) could be expresses as:
```sh
Log  2 (Add (Var "x") (Const 1))
```
4. unary trigonometry: sin, cos
5. unary natural logarithm: ln
6. wrappers for simple constants
7. string identifier for variables


The `getVars` finction could retrive variable identifiers from an expession and put these variable into a list without duplication.
```sh
>>> getVars (var "x" !+ var "y" !+ val 10)
["x","y"]
```


## ExprDiff

The module contains a type class `DiffExpr` and instances for differentiable expressions. 

The type class `DiffExpr` has :
- methods which need to be defined in instances:
   -  `eval`--  do evaluation
   - `simp` -- do some basic, first step simplification
   - `partDiff` -- do partial differention
   

- methods coming along with the class type, which are given by default:
   - `simplify` -- simplify the expression recursively based on the given `simp` rules untill it can't be reduced anymore
   - `!+` -- binary addition, takes 2 expressions, encodes addtion and simplification together,  and returns the resulting expr.
   - `!*` -- binary multiplaction, takes 2 expressions, encodes multiplation and simplification together, and returns the resulting expr.
   - `!^` -- exponentiation: takes an expr and a number, encodes exponentation and simplification together,  and returns the resulting expr.
   - `loga` -- logarithm of arbitory numerical base: takes a number as base and an expr, encodes logarithm and simplification together,  and returns the resulting expr.
   - `sine` -- unary sin op & simplification
   - `cosi` -- unary cos op & simplification
   - `ln` -- unary ln & simplification
   - `val` -- wrapper for simple values
   - `var` -- string identifier for variables


The instances are defined over 4 intuative Num type: Double, Float, Int, Integer.

As for `simplify` method, what you could expect it to do:
- substitute the variable identifier in the expr with the corresponding values given in the dictionary
- remove 0 item
- always make the constant item to the left-most
- add up all constants together in the expression no matter the order, and move the addition to the left-most.
- identify constants multiplied, and return the multiplication.
- do distribution law, commutative law 
 
For specific rules of simplification, check the file `SimpRules.log`



## ExprVector

The module contains a type class `VectorSpace` and instances for vector expressions.

The vectors are represented as a LIST of numbers, and the types of numbers include Double, Float, Int, Integer.

The type class `VectorSpace` has:
- methods which need to be defined in instances:
    - `evalV` -- evaluation for vector expr
    - `simpV` -- first step simplification for vector expr
- methods by default:
   - `simplifyV` -- eventual simplification
   - `?+` -- binary addition & simplification
   - `?*` -- binary dot production & simplifation
   - `?^` -- exponentiation & simplification
   - `logaV` -- logarithm of arbitory base & simplification
   - `sineV` -- unary sin & simplification
   - `cosiV` -- unary cos & simplification
   - `lnV` -- unary ln & simplification
   - `valV` -- wrapper for simple vectors
   - `varV` -- string identifier for variables


As for vector culculations:
1. The vevtors adding/multiplying together should be in the some size, or will return an error.
2. Exponentation will return an error: `Exp expr a` is not available in Vector, cuz a vector exponent does not make any sense.
3. Logarithm will return an error: `Log a expr` is not available in Vector, cuz a vector base does not make any sense.
4. Unary operation, sin, cos, ln, over vector `[v1, v2, v3, …]` equals to `[op v1, op v2, op v3, …]`


Hint:
When type vector expression directly in ghci, has to specify its type signature, or will confuse the ghci and cause ambiguous type error:
```sh
>>> varV "x" :: Expr [Int]
var "x"
```



## ExprParser
The module contains parsers to parse a formatted string into the corresponding expression datatype.

Language specification for Expr over Number:

| Type Encoding  | String Representation |
| ------ | ------ |
| val   1  | 1  |
| val 1.0 | 1.0 |
|var "x" |     x  |
|(val -1) !*  var "x" |     - x | 
|val 1.0 !+ val 2.0 |     1.0 + 2.0  |
|val 1 !+ ((val -1) * (var "x")) | 1 - x|
| val 3 !* var "x" |     3 * x |
| exp (var "x") 2  |     x ^ 2 |
| loga 10 (var "x") |     log 10 x |
| sine (val 1) | sin 1 |
| cosi (var "x")   |cos x|
| ln (var "x")  |   ln x |
| val 3 !* ((var "x") ^ -1.0) |     3/x   | 

Note: 
1. parse the minus as (-1) * expr; 
2. parse the division as expr ^ (-1)



Language specification for Expr over Vector:

| Type Encoding  | String Representation |
| ------ | ------  |
|valV   [1,2,3,4]  |     [1,2,3,4]  |
|valV   [-1,-2,-3,-4]|  - [1,2,3,4] |
|varV "x" |     x  |
|valV [1,2,3] ?+ valV [4,5,6] | [1,2,3] + [4,5,6] |
|varV "x" ?+ valV [-1,-2,-3,-4]| x - [1,2,3,4] |
|valV [1,2,3] ?* varV [4,5,6] | [1,2,3] * [4,5,6] |
| valV [1,2,3] ?* varV "x" | [1,2,3] * x |
|sineV (valV [1,2,3]) | sin [1,2,3]|
|cosiV (varV "x") |  cos x |
|lnV (varV "x") | ln x|
|error  |     ... ^ ... |
|error  |     log ...| 
|error |     ... / ... |

Note:
1. parse negative vector `- [v1, v2,...]` as `[-v1, -v2, ...]`
2. parse minus operation `expr - [v1, v2,...]` as `expr + [-v1,-v2,...]`
2. try parsing `/` will return an error, cuz division is not available in Vector 


## ExprPretty

The module contains the customer Show instance of expression data type and provides a nicely printing-out form to expressions:

- bracket the items of each step of operations
- make constructor `Add` printing-out form as `+`
- make constructor `Mult` printing-out form as `*`
- make constructor `Exp` printing-out form as `^`
- make constructor `Log` printing-out form as `log`
- make constructor `Sin` printing-out dorm as `sin`
- make constructor `Cos` printing-out form as `cos`
- make constructor `Ln` printing-out form as `ln`
- make constructor `Val` printing-out form as `val`
- make constructor `Var` printing-out form as `var`

e.g.
```sh
>>> (var "x" !^ 2) !+ (val 3 !* var "y")
((var "x") ^ 2) + ((val 3) * (var "y"))
```
```sh
>>> varV "x" ?* sineV (valV [1,2,3] :: Expr [Float])
(val [0.84147096,0.9092974,0.14112]) * (var "x")
```

## ExprTest

The module contains test cases and quickCheck methods over the 5 modules above.

To see the specific properties of quickCheck, check the file `quickCheck.txt`.


 ## References
 - [Abstract Nonsense](http://5outh.blogspot.ca/2013/05/symbolic-calculus-in-haskell.html) by Benjamin Kovach
 - [Symbolic Algebra](https://github.com/mrkgnao/pebble) by Soham Chowdhury
 - [Algo](https://github.com/neu-rah/algo) by Rui Azevedo



###### Copyright Meijing Li @ 2018





