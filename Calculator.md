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

The functions used in this section are documented on the Hackage
website, you may want to keep these open as tabs in your web browser.

* [Prelude]( https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html ): https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html
* [Data.Char]( https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html ): https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html

### 3.1. Define a function for removing leading whitespace
Please call this function `dropWS`. Use the `Prelude.dropWhile`
function and the `Data.Char.isSpace` function to define it. The
function prototype should be:

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
Please call this function `parseLabel`. Use the `Prelude.span`
function and the `Data.Char.isAlpha` function. The type of this
function should be:

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
Let's begin creating parsers for our `CalcAST` data type, beginning
with the `Literal` constructor.

Haskell's Prelude module provides standard parsers for data types like
`Double`, all under the `Prelude.reads` function. `reads` is
polymorphic, so the implementation of `reads` is different depending
on the type of the expression where it is used.

1. Please write a comment in your program indicating the type of the
   function `reads`
   
2. Please write a comment in your program indicating the type of the
   `ReadS` data type.
   
3. Write a function `parseLiteral` which uses `reads` to parse a
   literal value and wrap that value in a `Literal`
   constructor. `reads` will return an empty list if the input string
   does not match the parser. The type of `parseLiteral` should be:
   
   ``` haskell
   parseLiteral :: String -> [(CalcAST, String)]
   ```
   
Test this function on the following inputs and make sure the utputs
are correct (note that `Literal` in the output should match he
constructor you defined for `CalcAST`):

* `parseLiteral "1618 * 2"` should output `[(Literal 1618.0," * 2")]`

* `parseLiteral "-1618 * 2"` should output `[(Literal -1618.0," * 2")]`

* `parseLiteral "1.618 * 2"` should output `[(Literal 1.618," * 2")]`

* `parseLiteral "-1.618 * 2"` should output `[(Literal -1.618," * 2")]`

* `parseLiteral "1618e3 * 2"` should output `[(Literal 1618000.0," * 2")]`

* `parseLiteral "1.618e3 * 2"` should output `[(Literal 1618.0," * 2")]`

* `parseLiteral "1618e-3 * 2"` should output `[(Literal 1.618," * 2")]`

* `parseLiteral "1.618e+3 * 2"` should output `[(Literal 1618.0," * 2)]`

* `parseLiteral "-1.618e+3 * 2"` should output `[(Literal -1618.0," * 2)]`

* `parseLiteral " -1.618e+3 * 2"` should output `[]`, this should fail
  because the string begins with whitespace.

* `parseLiteral ".618"` should output `[]`, this should fail because
  we don't care about numbers starting with a decimal point.

* `parseLiteral "a.618"` should output `[]`, this should fail because
  numbers must not begin with alphabetic characters.

### 3.4. Define a function for choosing between `parseLiteral` and `parseLabel`
This function should take functions as parameters, also known as a
"higher order function" (refer to chapter 6 of the "[Learn You a
Haskell for Great Good]( http://learnyouahaskell.com/ )" textbook for
more information).

Our function should be called `parseChoice` and should have the type:

``` haskell
parseChoice :: ReadS a -> ReadS a -> ReadS a
```

So `parseChoice` should take two parsing functions of type `ReadS`,
function `a` and function `b`. If function `a` returns an empty list,
it should return the result of `b` instead. (**Hint:** can you define
this function using the `++` operator?)

**NOTE** that the two input arguments to `parseChoice` both have type
`ReadS a`, this means the `a` parse and the `b` parser must both
return **the same type** of value. `parseChoice` will NOT be able to
parse a choice between `ReadS Double` and `ReadS String`, and this is
why it is necessary to wrap the output of these functions in our
`CalcAST` data type.

For now, make sure this function type checks, we will test in in the
upcoming exercises.

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

#### 3.5.1. Rewrite `dropWS` as a `ReadS` type function.
The new `dropWS` function should never fail (never return an empty
list), and it should be of type:

``` haskell
dropWS :: String -> ReadS ()
```

It may seem unnecessary to wrap the output in a tuple with an empty
value, but this will make `dropWS` easier to use in higher order
functions later on.

### 3.6. Define a function for parsing parenthetical expressions.
Haskell's Prelude module provides a standard parser for parenthetical
expressions called `Prelude.readParen`. Be sure to lookupt the
documentation for how `readParen` works.

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

## 4. Handling parser errors
Using `ReadS` is a good way to throw together a quick parser, but it
doesn't tell us very much about what went wrong. Haskell provides a
function `Prelude.error` for throwing an exception, however this will
bring your entire program to a crashing halt.

A more gentle way of reporting errors is to wrap function results in
an 'Either' data type, with `Left` indicating an error condition, and
`Right` indicating success.

The `Either` data type is both a Monad, and an Applicative functor, so
you can use it with `do` notation.

It is good practice to use descriptive names for types, so let's
define a type synonym called `ErrorMessage` which is simply a
descriptive way of indicating a `String` type:

``` haskell
type ErrorMessage = String
```

### 4.1 Define a `Parser` type which uses `Either`.
Recall that what type of `ReadS` is:

``` haskell
type ReadS any = String -> [(any, String)]
```

Notice that the type variable `any` is used to indicate that the type
may produce results of different data types. For example, when `any`
is `Int`, we must have parsed an integer value from the input
string. When `any` is `(Int, String)`, we must have parsed a tuple
containing an integer and a string.

Let's create our own parser type that is similar to `ReadS`:

``` haskell
type Parser any = ...
```

As with `ReadS`, this parser type should be a function that always
takes a `String` as input. **Unlike** `ReadS` our `Parser` should
returns an `Either` type as it's result instead of a list. The
`Either` type should evaluates to an `ErrorMessage` (a `String`) when
the `Left` constructor is used, and evaluates to the a tuple
containing the variable `any` type as the first tuple item, and the
unparsed string remainder as the second tuple item.

The answer to this exercise is to define is the type `Pasrer` as
described.

### 4.2. Write a function that converts a `ReadS` to our own `Parser` type
Haskellers conventionally refer to functions which convert higher
order-functions to even *higher* higher-order functions as "lifting"
functions. So let's call this conversion function `liftReadS`. The
type of `liftReadS` should be:

``` haskell
liftReadS :: ErrorMessage -> ReadS any -> Parser any
```

As you can see, `liftReadS` is a higher-order function that takes
`ReadS` as an argument which we will convert. Let's call the function
the "argument function", because it is taken as an argument.

The `liftReadS` function should behave like so:

* If the `ReadS` argument function returns an empty list, `liftReadS`
  should return a `Left` containing the `ErrorMessage`.

* If the `ReadS` argument function returns a list that contains
  **exactly one** result tuple, return that result tuple in a `Right`
  constructor.

* If the `ReadS` argument function returns multiple results, return a
  `Left` containing the error message. Prepend to the error message
  string the message `"<ambiguous parse> "`.

### 4.3. Rewrite all of our `ReadS` function as the `Parser` type
This is going to take a bit of work.

**BUT** this is one of Haskell's greatest strengths. When you make a
change the types within your program, Haskell's type checker will
notify, in the form of error messages, every part of your program that
is effected by the change. Many Haskellers have reported that they
feel safe making incremental changes to their program precisely
because of this feature of the programming language.

Make sure all parsing functions we have written so far have the
following types:

``` haskell
dropWS :: Parser () -- never fails

parseLiteral :: Parser CalcAST -- can use 'liftReadS' for this

parseLabel :: Parser CalcAST

parseParen :: Parser CalcAST -- can use 'liftReadS' for this

parseCalc :: Parser CalcAST

parseChoice :: Parser a -> Parser a -> Parser a
```

### Recommendation:
I strongly recomend you **only** change the types of these functions
at first without rewriting any other part of your code, and then try
to recompile. The `ghc` compiler will produce a long list of compiler
error messages. Use these error messages to your advantage; these
errors are Haskell's way telling you how to change your program
safely.

### Hint: use `liftReadS` to convert functions that use `reads`
Don't forget to use `liftReadS` that we defined in exercise 4.2 in
every function where the `Prelude.reads` function is used. You may use
`liftReadS` to convert all of your functions, but `parseLabel` and can
probably easily be re-written without `liftReadS`. You will not be
able to use `liftReadS` to convert `parseCalc` or `parseChoice`.

## 4.4. Write `parseChar` and `parseSymbol`

## 4.5. Use `fmap` and `parseSymbol` to re-write `parseLabel`

## 4.6. Write the `parseFunc` function for parsing "sin", "cosh", "exp", and "log"

# 5. Turn `Parser` into our own custom monad
