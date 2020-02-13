# Overview
In the following exercises, we will learn how to create parsers for a
very simple arithmetic calculator language in Haskell.

In these exercises, students will learn about the basics of "parser
combinators" by creating our own simple parser combinator
library. This will give us a deep understanding of how industrial
parser combinator libraries such as Attoparsec or Megaparsec work
internally.

We will begin by creating a naive parser, then iteratively upgrade our
program. After each iteration our program will begin to take the form
of a more proper and eloquent Haskell parser. By "eloquent" I mean we
will use more advanced concepts (e.g. Monads and Applicative Functors)
to make our program much shorter, and therefore easier to change and
improve.

My hope is that students following these exercises will be able to
understand the advanced concepts works "under the hood," because we
will (hopefully) understand how the eloquent code is equivalent to the
more simplistic, naive code.

This document provides no instruction, rather students are expected to
search the Internet, or use a Haskell textbook which they already own,
for resources on how to solve these exercises. For a free online
textbook, I recommend [Learn You a Haskell for Great Good](
http://learnyouahaskell.com/ ).

We will be using functions defined in the Haskell "Prelude" library
which provides API functions compliant with the Haskell 2010 language
standard. The documentation for Prelude is here:

* https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html
* https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html

We will use the `ghc --make` command to build our program. Please
create a new Haskell source file `Calc.hs`.

We will be making use of the standard character predicates module
`Data.Char`.

Open `Calc.hs` in your editor declare it to be our Main program, and
import the `Data.Char` library

``` haskell
module Main where

import Data.Char
```

## A note about this Git repository
I will write the answers to exercises in a file called
`Calculator.hs`, which will **NOT** be on the `master` branch. For
example, to view the `Calculator.hs` program that answers the exercise
problem 3.1., check out the branch with the name `ex-3.1` using the
Git command line:

``` sh
git checkout ex-3.1
```

**NOTE** that this is a work in progress, not all the exercises may
have answers yet.

Check out the branch `all-answers` to see the final program. You can
use the `git log` command to view the history of changes to the final
`Calculator.hs` program, each commit in the history will be an answer
to an exercise problem.

## 1. Define an Abstract Syntax Tree (AST)
Define a data type that will serve as our AST. Let's call this data
type `CalcAST`. The `CalcAST` datatype should derive the `Show` and
`Read` typeclasses.

The data type will have 5 different constructors:

- **Literals:** a constructor for literal integers. The constructor should
  have only 1 field of type `Double` (a double-precision floating
  point number).

- **Labeled constants:** A constructor for constant values such as
  "pi". This constructor should contain only 1 field of type `String`
  which will contain the name of constant value.

- **Infix operators:** A constructor which contains an infix operator
  The infix operator should contain a single character such as "+",
  "-", "*". The constructor should have 3 fields. Two of the fields
  should contain a value of type `CalcAST` -- this will represent the
  operands on either side of the infix operator. One of the fields
  should be of type `Char` representing the infix operator character.

- **Labeled functions:** A constructor for functions like "sin",
  "cosh", "exp", or "log". This constructor must have two fields. The
  first field should be of type `String` and will represent the name
  of the function ("sin", "cosh", "exp", "log"), the second field must
  be of type `CalcAST` which will contain the operand of the function.

- **Parenthetical expressions:** A constructor for parentheses. This
  constructor should contain a single field of type `CalcAST` which
  contains the sub-expression that is written in parentheses.

## 2. Define an evaluator
Let's define a function called `calcEval`. The type of this function
should be:

``` haskell
calcEval :: CalcAST -> Either ErrorMessage Double

type ErrorMessage = String
```

To get you started, you can begin defining `calcEval` like so:

``` haskell
calcEval :: CalcAST -> Either ErrorMessage Double
calcEval expr = case expr of
    Literal a  -> Right a
    Label "pi" -> Right pi
    Label sym  -> Left ("unknown symbol " ++ show sym)
```

Be sure `calcEval` works for all possible `CalcAST` constructors.

You can write a simple test program like so:

``` haskell
testExpressions :: [CalcAST]
testExpressions =
    [ Literal 1.618
    , Label "pi"
    , Label "Nami's Astrological Constant"
    , ...more tests...
    , ...even more tests...
    ]

main :: IO ()
main = mapM_ print (fmap (\ test -> (test, calcEval test)) testExpressions)
```

## 3. Define a naive parser

For our parsing functions, we will be working with lists of characters
`[Char]` (which is synonymous with the type `String`), so we can use
some of Haskell's standard list functions.

The following functions can all be written with 1 line of code, so I
recommend you use a Haskell REPL such as GHCI or the IPython Haskell
notebook to write these functions. Once you have a function that
works, copy and paste it from the REPL into your source code file.

### 3.1. Define a function for removing leading whitespace
Please call this function `dropWS`. Use the `dropWhile` function and
the `Data.Char.isSpace` function to define it. The function prototype
should be:

``` haskell
dropWS :: String -> String
```

Test this function on the following inputs and make sure the outputs
are correct:

* `dropWS "   hello"` should output `"hello"`

* `dropWS "hello"` should output `"hello"`. If the input does not
  start with whitespace, the function should return the string
  unchanged.

### 3.2. Define a function for parsing constant values such as "pi". 
Please call this function `parseLabel`. Use the `span` function and the
`Data.Char.isAlpha` function. The type of this function should be:

``` haskell
parseLabel :: String -> (String, String)
```

Test this function on the following inputs and make sure the outputs
are correct:

* `parseLabel "pi * 2"` should output `("pi", " * 2")`

* `parseLabel "2 * pi"` should output `("", "2 * pi")`. This should
  not parse anything because the input string begins with a number,
  not a label.

* `parseLabel " pi"` should output `("", " pi")`. This should not
  parse anything becuase the input string begins with a whitespace,
  not a label.

### 3.3. Define a function for parsing literal numbers such as "1.6182"
Haskell's Prelude module provides standard parsers for data types like
`Double`, all under the `reads` function. `reads` is polymorphic, so
the implementation of `reads` is different depending on the type of
the expression where it is used.

1. Please write a comment in your program indicating the type of the
   function `reads`
   
2. Please write a comment in your program indicating the type of the
   `ReadS` data type.
   
3. Write a function `parseLiteral` which uses `reads` to parse a
   literal. `reads` will return an empty list if the input string does
   not match the parser. The type of `parseLiteral` should be:
   
   ``` haskell
   parseLiteral :: String -> [(CalcAST, String)]
   ```
   
   Test this function on the following inputs and make sure the
   outputs are correct (note that `Literal` in the output should match
   the constructor you defined for `CalcAST`):

    * `parseLiteral "1618 * 2"` should output `[(Literal 1618.0," * 2")]`
    
    * `parseLiteral "-1618 * 2"` should output `[(Literal -1618.0," * 2")]`

    * `parseLiteral "1.618 * 2"` should output `[(Literal 1.618," * 2")]`

    * `parseLiteral "-1.618 * 2"` should output `[(Literal -1.618," * 2")]`

    * `parseLiteral "1618e3 * 2"` should output `[(Literal 1618000.0," * 2")]`

    * `parseLiteral "1.618e3 * 2"` should output `[(Literal 1618.0," * 2")]`

    * `parseLiteral "1618e-3 * 2"` should output `[(Literal 1.618," * 2")]`

    * `parseLiteral "1.618e+3 * 2"` should output `[(Literal 1618.0," * 2)]`

    * `parseLiteral "-1.618e+3 * 2"` should output `[(Literal -1618.0," * 2)]`

    * `parseLiteral " -1.618e+3 * 2"` should output `[]`, this should
      fail because the string begins with whitespace.

    * `parseLiteral ".618"` should output `[]`, this should fail
      because we don't care about numbers starting with a decimal
      point.

    * `parseLiteral "a.618"` should output `[]`, this should fail
      because numbers must not begin with alphabetic characters.

### 3.4. Define a function for choosing between `parseLiteral` and `parseLabel`
This function should take functions as parameters, also known as a
"higher order function" (refer to chapter 6 of the "Learn You a
Haskell for Great Good" textbook for more information).

Our function should be called `parseChoice` and should have the type:

``` haskell
parseChoice :: ReadS any -> ReadS any -> ReadS any
```

So `parseChoice` should take two parsing functions of type `ReadS`,
function `a` and function `b`. If function `a` returns an empty list,
it should return the result of `b` instead.

### 3.5. Rewrite the `parseLabel` function so it can be used easily in `parseChoice`
The `parseLabel` function we wrote in exercise 1.2.2 was of type:

``` haskell
parseLabel :: String -> (String, String)
```

But it would be easier to use this function with `parseChoice`
(defined above in exercise 1.2.4) if the type of `parseLabel` were:

``` haskell
parseLabel :: ReadS CalcAST

-- Recall that the type of `ReadS any` is `String -> [(any, String)]`.
```

Lets rename the old `parseLabel` function to `oldParseLabel` and write
a new function `parseLabel`. The new `parseLabel` should take the
output of `oldParseLabel` and return an empty list if the output of
`parseLabel` is `("", __anythingElse__)`. If `oldParseLabel` returns a
non-empty string, `parseLabel` should return the result in a list as
the only element of that list.

Test our new `parseLabel` function by using it with `parseChoice`,
test it on the following inputs and make sure the outputs are correct:

* `parseChoice parseLabel parseLiteral "abc 123"` should return
  `[(Label "abc"," 123")]`
    
* `parseChoice parseLabel parseLiteral "123 abc"` should return
  `[(Literal 123.0," abc")]`

* `parseChoice parseLabel parseLiteral ".123 abc"` should return `[]`,
  this should fail because ".123" is not a valid literal number, nor
  is it a valid label.

* `parseChoice parseLabel parseLiteral " 123 abc"` should return `[]`
  because the input string begins with whitespace and neither
  `parseLabel` nor `parseLiteral` should return a successful result.

### 3.6. Define a function for parsing parenthetical expressions.
Haskell's Prelude module provides a standard parser for parenthetical
expressions called `readParen`. Be sure to lookupt the documentation
for how `readParen` works.

**Note** that `readParen` will automatically remove whitespaces inside
of the parentheses for you, so there is no need to use `dropWS`.

Write a function called `parseParen` which uses `readParen`, and pass
`parseChoice parseLabel parseLiteral` as the expression to be parsed
by `readParen`. The type of `parseParen` should be:

``` haskell
parseParen :: ReadS CalcAST
```

Test this function on the following inputs and make sure the outputs
are correct:

* `parseParen "( abc )"` should output `[(Paren (Label "abc"),"")]`

* `parseParen "( 123 )"` should output `[(Paren (Literal 123.0, "")]`

* `parseParen "( .123 )"` should output `[]`, this should fail because
  ".123" is not a valid literal number or label.

* `parseParen " 123)"` should output `[]`, this should fail because
  the expression does not begin with a parenthesis.

* `parseParen "( 123"` should output `[]`, this should fail because
  the expression does not have balanced parenthesis.

### 3.7. Recursively parsing parentheses
Now we want to be able to parse nested parentheses. Make use of the
`parseChoice` function to chose between parsing an expression with
parentheses and an expression without parentheses. Write a function
called `parseExpr`. The type of `parseExpr` should be:

``` haskell
parseExpr :: ReadS CalcAST
```

Test this function on the following inputs and make sure the outputs
are correct:

* `parseCalc "(( abc ))"` should output `[(Paren (Paren (Label "abc")),"")]`

* `parseCalc "(( 123 ))"` should output `[(Paren (Paren (Literal 123.0)),"")]`

* `parseCalc "(( 123 )))"` should output `[(Paren (Paren (Literal 123.0)),")")]`

* `parseCalc "((( 123))" `should output `[]`, this should fail because
  there are not enough closing parenthesis.

