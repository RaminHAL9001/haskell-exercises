## 1. Define an Abstract Syntax Tree (AST)
Define a data type that will serve as our AST. Let's call this data
type `CalcAST`. The `CalcAST` datatype should derive the `Eq`, `Show`,
and `Read` typeclasses.

The data type will have 5 different constructors:

- **Literals:** a constructor called `Literal` for representing
  literal number values. The constructor should have only 1 field of
  type `Double` (a double-precision floating point number).

- **Labeled constants:** A constructor called `Label` which represents
  constant values such as "pi". This constructor should contain only 1
  field of type `String` which will contain the name of constant
  value.

- **Infix operators:** A constructor called `Infix` which represents
  an infix operator The infix operator should contain a single
  character such as "+", "-", "*". The constructor should have 3
  fields. Two of the fields should contain a value of type `CalcAST`
  -- this will represent the operands on either side of the infix
  operator. One of the fields should be of type `Char` representing
  the infix operator character.

- **Labeled functions:** A constructor called `Function` for
  representing functions like "sin", "cosh", "exp", or "log". This
  constructor must have two fields. The first field should be of type
  `String` and will represent the name of the function ("sin", "cosh",
  "exp", "log"), the second field must be of type `CalcAST` which will
  contain the operand of the function.

- **Parenthetical expressions:** A constructor called `Paren` for
  representing expressions stored in parentheses. This constructor
  should contain a single field of type `CalcAST` which contains the
  sub-expression that is written in parentheses.

For more information on how to do this, refer to [chapter 8 of Learn
You a Haskell for Great Good](
http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types
).

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
    Literal lit  -> Right lit
    Label   "pi" -> Right pi
    Label   sym  -> Left ("unknown symbol " ++ show sym)
```

You can write a simple test program like so:

``` haskell
testExpressions :: [CalcAST]
testExpressions =
    [ Literal 1.618
    , Label "pi"
    , Label "Nami's Cosmological Constant"
    ]

main :: IO ()
main = mapM_ print
    (fmap (\ test -> (test, calcEval test)) testExpressions)
```

Compile this program. You will get warning messages which you can
ignore for now.

For more information on how to do the exercises in this section, refer
to [chapter 4 of Learn You a Haskell for Great Good](
http://learnyouahaskell.com/chapters ).

### 2.1. Write a function to convert a `String` like "pi" to a `Double` value.
Call this function `getConstant`, and should have this type:

``` haskell
getConstant :: String -> Either ErrorMessage Double
```

Define this function to return a value of `Right pi` when given the
string `"pi"`, return a value of `Right (exp 1.0)` when given the
string "e", return a value of `Left` with an error message string
"unknown symbol", append the unknown symbol in as part of the error
message.

### 2.2. Write a function to convert an operator `Char` to an arithmetic function.
When writing an evaluator for the `Infix` constructor, we need to
produce a built-in arithmetic function such as `(+)` for a character
representing that function, such as `'+'`.

Write a function `getArithmetic` which should have the function type:

``` haskell
getArithmetic :: Char -> Either ErrorMessage (Double -> Double -> Double)
```

This function should return a `Right` constructor containing each of
the functions `(+)`, `(-)`, `(*)`, and `(/)` if the `Char` argument
matches the associated character representation of these functions. If
an unknown `Char` value is

The data type `Either` is a kind of monadic function type, which means
we can use functions like our `getArithmetic` function using `do`
notation.

Write your `calcEval` function for the `Infix` constructor to make use
of `do` notation, use the `getArithmetic` function to convert the
character opcode field fo the `Infix` constructor to an arithmetic
function, then apply the function to each of the `CalcAST` operands in
the `Infix` constructor.

Refer to [chapter 12 of Learn You a Haskell for Great Good](
http://learnyouahaskell.com/a-fistful-of-monads#do-notation ) for more
details about `do` notation.

**Hint:**

``` haskell
calcEval expr = case expr of
    Infix opcode a b -> do            --- <- Here is the 'do' keyword,
        oper <- getArithmetic opcode  --- <- here we use our 'getArithmetic' function.
        ... what goes here? ...
```

### 2.3. Use a type synonym to define a type of `Either ErrorMessage`
We have already written `getConstant` in 2.1 and `getArithmetic` in
2.2, both returning a type `Either ErrorMessage something` where
`something` is a `Double` value or a function of type `(Double ->
Double -> Double)`.

We will be writting many more functions that evaluate to `Either
ErrorMessage something`, so write a new type synonym called `Evaluate`
that we can write instead so we don't have to do as much typing.

``` haskell
type Evaluate a = ...
```

Rewrite the `getConstant`, `getArithmetic`, and `calcEval` functions
defined above in exercise 2.1, 2.2, and 2 (repsectively) to have the
following type signatures:

``` haskell
getConstant :: String -> Evaluate Double

getArithmetic :: Char -> Evaluate (Double -> Double -> Double)

calcEval :: CalcAST Evaluate Double
```

Refer to [chapter 8 of Learn You a Haskell for Great Good](
http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-synonyms
).

**Note** that you will still be able to use `do` notation as before,
you are only renaming these function type, not changing their types.

### 2.4. Write a function for evaluating trigonometrics and logarithmics
The function should be called `getFunction` and hav a type of:

``` haskell
getFunction :: String -> Evaluate (Double -> Double)
```

This function should be defined for `sin`, `cos`, `tan`, `sqrt`,
`exp`, `log`. If you want, you can also define it for `asin`, `acos`,
`atan`, `sinh`, `cosh`, `tanh`.

As with the `getConstant` function (exercise 2.1) and the
`getArithmetic` functino (exercise 2.2), report a useful error message
if the function name passed to `getFunction` is unknown.

Update the `calcEval` function to use `getFunction`, use `do` notation
as you did in exercise 2.2.

### 2.5. Use applicative functor notation to evaluate `Infix` expressions
You may have been tempted to write `calcEval` for the `Infix`
constructor using only case statements:

First, at the top of the `Calculator.hs` file, write `import
Control.Applicative`. This will provide to you the (`<$>`) and (`<*>`)
operators.

``` haskell
calcEval :: CalcAST -> Either ErrorMessage Double
calcEval expr = case expr of
    Infix opcode a b -> do
        oper <- getArithmetic opcode
        case calcEval a of
            Left err -> Left err
            Right  a -> case calcEval b of
                Left err -> Left err
                Right  b -> Right (oper a b)
```

If this was similar to your solution, you will be happy to learn that
there is a much easier way.

If you were very clever, you may have realized that `calcEval` is a
function of type `Either` and therefore you can use it with `do`
notation, calling `calcEval` recursively like so:

``` haskell
calcEval :: CalcAST -> Either ErrorMessage Double
calcEval expr = case expr of
    Infix opcode a b -> do
        oper <- getArithmetic opcode
        a <- calcEval a
        b <- calcEval b
        Right (oper a b)
```

However, there is an even more concise way to do this: using the
applicative functor operators `(<$>)` and `(<*>)`, provided by the
`Control.Applicative` module can be used. Try the following in GHCI:

``` haskell
(+) <$> Right 5.0 <*> Right 2.0
(-) <$> Right 5.0 <*> Right 2.0
(*) <$> Right 5.0 <*> Right 2.0
(+) <$> Left "left-operand error" <*> Right 2.0
(+) <$> Right 5.0 <*> Left "right-operand error"
(+) <$> Left "left-operand error" <*> Left "right-opearand error"
```

Use this technique to define `calcEval` for the `Infix` constructor,
and for the `Function` constructor where you use the `getFunction`
function we defined in exercise 2.4.

The `(+)` syntax is called the "function section" feature, which is
discussed in [chapter 6 of Learn You a Haskell for Great Good](
http://learnyouahaskell.com/higher-order-functions ).

Review [chapter 11 of Learn You a Haskell for Great Good](
http://learnyouahaskell.com/functors-applicative-functors-and-monoids
), for more information about applicative functors.

### 2.6. Finish writing `calcEval`
Make sure the `calcEval` is defined for every data constructor of the
`CalcAST` data type. Copy the following test code into
`Calculator.hs`, and then build and run the program.

``` haskell
data TestCase input result
    = TestCase
      { input    :: input
      , expected :: result
      , actual   :: result
      }
    deriving (Eq, Show)

testCase
    :: (Eq result, Show input, Show result)
    => (input -> result) -> [(input, result)] -> IO ()
testCase runTestFunction = mapM_ $ \ (exampleInput, expectedResult) ->
    let result = TestCase
            { input    = exampleInput
            , expected = expectedResult
            , actual   = runTestFunction exampleInput
            }
    in if expected result == actual result
        then return ()          -- Do nothing if the parser is successful,
        else fail (show result) -- otherwise halt the program with an error.

-- | The 'AlwaysEqual' type is used to wrap values such that they can be shown
-- with 'show', but when they are compared using '(==)', they are always equal.
newtype AlwaysEqual a = AlwaysEqual a
instance Show a => Show (AlwaysEqual a) where { show (AlwaysEqual a) = show a; }
instance Eq (AlwaysEqual a) where { _ == _ = True; }

leftEq :: Either err any -> Either (AlwaysEqual err) any
leftEq result = case result of
    Left err -> Left (AlwaysEqual err)
    Right  a -> Right a

calcTest :: [(CalcAST, Evaluate Double)] -> IO ()
calcTest expectedResults =
    testCase (leftEq <$> calcEval) (fmap leftEq <$> expectedResults)

main :: IO ()
main = do
    let ok = return
    let nope = Left ""
    calcTest
        [ (Literal 1.618, ok 1.618)
        , (Literal 0, ok 0)
        , (Label "pi", ok pi)
        , (Label "e", ok (exp 1.0))
        , (Label "Nami's Cosmological Constant", nope)
        , (Paren (Literal 1.618), ok 1.618)
        , (Paren (Label "FooBar"), nope)
        , (Paren (Paren (Literal 1.618)), ok 1.618)
        , (Paren (Paren (Paren (Label "FooBar"))), nope)
        , (Infix '+' (Literal 1) (Literal 2), ok 3.0)
        , (Infix '+' (Literal 5) (Infix '*' (Literal 2) (Literal 3)), ok 11.0)
        , (Infix '*' (Paren (Infix '+' (Literal 5) (Literal 2))) (Literal 3), ok 21.0)
        , (Infix '/' (Infix '*' (Literal 2) (Label "pi")) (Literal 2), ok pi)
        , (Infix '+' (Infix '&' (Literal 0) (Literal 1)) (Literal 2), nope)
        , (Function "sqrt" (Literal 2), ok (sqrt 2))
        , (Function "sin" (Paren (Infix '*' (Literal 2) (Label "pi"))), ok (sin (2*pi)))
        , (Paren (Function "log" (Paren (Function "exp" (Literal 1)))), ok (log (exp 1.0)))
        ]
    -- If any of the tests above failed, the program will have halted
    -- by evaluating 'fail'. If all tests passed, this final line of
    -- code is executed.
    putStrLn "All tests passed."
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

### 3.2. Define a function for parsing constant values such as "pi"
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
   
   ``` haskell   parseLiteral :: String -> [(CalcAST, String)]
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

* `parseLiteral ".618"` should output `[]`, this should fail because
  we don't care about numbers starting with a decimal point.

* `parseLiteral "a.618"` should output `[]`, this should fail because
  numbers must not begin with alphabetic characters.

### 3.4. Define a function for choosing between `parseLiteral` and `parseLabel`
This function should take functions as parameters, also known as a
"higher order function" (refer to [chapter 6 of Learn You a Haskell
for Great Good]( http://learnyouahaskell.com/ ) textbook for more
information).

Our function should be called `parseChoice` and should have the type:

``` haskell
parseChoice :: ReadS a -> ReadS a -> ReadS a
```

So `parseChoice` should take two parsing functions of type `ReadS`,
function `a` and function `b`. If function `a` returns an empty list,
it should return the result of `b` instead. (**Hint:** can you define
this function using the `++` operator?)

**NOTE** that the two input parameters to `parseChoice` both have type
`ReadS a`, this means the `a` parse and the `b` parser must both
return **the same type** of value. `parseChoice` will NOT be able to
parse a choice between `ReadS Double` and `ReadS String`, and this is
why it is necessary to wrap the output of these functions in our
`CalcAST` data type.

For now, just compile this program and make sure this function type
checks, we will test in in the upcoming exercises.

### 3.5. Rewrite the `parseLabel` function so it can be used easily in `parseChoice`
The `parseLabel` function we wrote in exercise 3.2 was of type:

``` haskell
parseLabel :: String -> (String, String)
```

But it would be easier to use this function with `parseChoice`
(defined above in exercise 3.4) if the type of `parseLabel` were:

``` haskell
parseLabel :: ReadS CalcAST

-- Recall that the type of `ReadS any` is `String -> [(any, String)]`.
```

Likewise, change the type of `parseLiteral` (from exercise 3.3) from
this:

``` haskell
parseLiteral :: String -> [(CalcAST, String)]
```

so that it is defined as a 'ReadS' prect function.

Lets rename the old `parseLabel` function to `oldParseLabel` and write
a new function `parseLabel`. The new `parseLabel` should take the
output of `oldParseLabel` and return an empty list if the output of
`parseLabel` is `("", __anythingElse__)`. If `oldParseLabel` returns a
non-empty string, `parseLabel` should return the result in a list as
the only element of that list.

Test our new `parseLabel` function by using it with `parseChoice`,
test it on the following inputs and make sure the outputs are correct:

#### 3.6. Rewrite `dropWS` as a `ReadS` type function
The new `dropWS` function should never fail (never return an empty
list), and it should be of type:

``` haskell
dropWS :: String -> ReadS ()
```

It may seem unnecessary to wrap the output in a tuple with an empty
value, but this will make `dropWS` easier to use in higher order
functions later on.

### 3.7. Create `parseExpr` which parses a choice between `parseLabel` and `parseLiteral`
This function should use the `parseChoice` function we defined in
exercise 3.4. The type of this function should be:

``` haskell
parseExpr :: ReadS CalcAST
```

* `parseExpr "abc 123"` should return `[(Label "abc"," 123")]`
    
* `parseExpr "123 abc"` should return `[(Literal 123.0," abc")]`

* `parseExpr ".123 abc"` should return `[]`, this should fail because
  ".123" is not a valid literal number, nor is it a valid label.

* `parseExpr " abc 123"` should return `[]` because the input string
  begins with whitespace followed by a label, and neither `parseLabel`
  nor `parseLiteral` should return a successful result.

### 3.8. Define a function for parsing parenthetical expressions
Haskell's Prelude module provides a standard parser for parenthetical
expressions called `Prelude.readParen`. Be sure to lookup the
documentation for how `readParen` works.

Try parsing with `readParens` the following expression in GHCI:

``` haskell
readParens True (reads :: ReadS Int) "(1)"
readParens True (reads :: ReadS Int) "()"
readParens True (reads :: ReadS Int) "((1))"
readParens True (reads :: ReadS Int) "(((1)))"
```

**IMPORTANT NOTE:** `readParen` has what is perhaps counter-intuitive
behavior in that it will parse nested parentheses and return multiple
results, the first result having only matched the outer-most
parentheses. Therefore it is necessary to ensure the `readParen`
function only returns a list of zero or one results. Use the
`Prelude.take` function to accomplish this.

Furthermore `readParen` will automatically remove whitespaces inside
of the parentheses for you, so there is no need to use `dropWS`.

Write a function called `parseParen` which uses `readParen`, and use
`parseExpr` as the expression to be parsed by `readParen`. The type of
`parseParen` should be:

``` haskell
parseParen :: ReadS CalcAST
```

Test this function on the following inputs and make sure the outputs
are correct:

* `parseParen "(abc)"` should output `[(Paren (Label "abc"),"")]`

* `parseParen "(123)"` should output `[(Paren (Literal 123.0, "")]`

* `parseParen "(.123)"` should output `[]`, this should fail because
  ".123" is not a valid literal number or label.

* `parseParen " 123)"` should output `[]`, this should fail because
  the expression does not begin with a parenthesis.

* `parseParen "(123"` should output `[]`, this should fail because
  the expression does not have balanced parenthesis.

### 3.9. Recursively parsing parentheses
Now we want to be able to parse nested parentheses. Make use of the
`parseChoice` function to chose between parsing an expression with
parentheses and an expression without parentheses. Write a function
called `parseCalc`. The type of `parseCalc` should be:

``` haskell
parseExpr :: ReadS CalcAST
```

You will need to make use of the `parseChoice` function we defined in
exercise 3.4. You will also need to modify `parseParen` to perform a
mututally recursive function call to `parseExpr`.

**IMPORTANT NOTE:** the `reads` parser which is used by `parseLiteral`
will parse it's own parentheses, so you will need to **also** modify
the `parseLiteral` function so that it rejects strings (returns an
empty list for input strings) that begin with parentheses or white
spaces. This will prevent `reads` from removing parentheses and giving
our own `parseParen` function defined in exercise 3.8 a chance to
parse the parentheses from the input string.

Test this function on the following inputs and make sure the outputs
are correct:

* `parseCalc "abc"` should output `[(Label "abc", "")]`

* `parseCalc "1618.0e-3"` should output `[(Literal 1.618, "")]`

* `parseCalc "((abc))"` should output `[(Paren (Paren (Label "abc")),"")]`

* `parseCalc "((123))"` should output `[(Paren (Paren (Literal 123.0)),"")]`

* `parseCalc "((123)))"` should output `[(Paren (Paren (Literal 123.0)),")")]`

* `parseCalc "(((123))" `should output `[]`, this should fail because
  there are not enough closing parenthesis.

### 3.10. Writing an infix operator parser with `do` notation
As we have learned already in exercise 2.2, any monadic function type
can be used with `do` notation. In exercise 2.2, we use an `Either`
function type with `do` notation.

But in Haskell, lists are monadic function types as well. Recall the
type of `ReadS` is:

``` haskell
type ReadS a = String -> [(a, String)]
```

What this means is, for any parser `p :: ReadS a`, if we write an
expression in which we apply an input string `inStr` to a parser `p`,
the type this parsing expression becomes `(p inStr :: [(a, String)])`,
which is a list, and therefore a monad, and therefore can be used with
`do` notation.

Create a function called `parseInfix`. Use the `parseCalc` function we
wrote in exercise 3.9, and the `dropWS` function we rewrote as a
`ReadS` parser in exercise 3.6 to create a parser for the 'Infix'
constructor of the `CalcAST` data type. The `parseInfix` function
should parse any valid infix operator (any of `'+'`, `'-'`, `'*'`, or
`'/'`).

**Do not worry** about mathematical operator precedence. In
mathematics, `3 + 2 * 1` should be equal to `3 + (2 * 1)` according to
the standard order of operations. However for this exercise,
`3 + 2 * 1` need not be any different from `(3 + 2) * 1`. It will be
much easier for the moment to treat all operators as the same fixity
precedence.

**Do not worry** about associativty yet either. In mathematics,
`4 + 3 + 2 + 1` should technically be equal to `((4 + 3) + 2) + 1`, but
it is OK for the purpose of this exercise for the expression to be
parsed to `4 + (3 + (2 + 1))`.

The type of our infix parser should be:

``` haskell
parseInfix :: ReadS CalcAST
```

Update the `parseParen` function so that it parses an infix expression
within the parentheses.

**Hint:** use the `getArithmetic` function to check if a character is
an acceptable infix operator. You will not need to use the function
returned by `getArithmetic` as we do in the evaluator, just check if
it returns `Right` or `Left`.

Test this parser on the following inputs:

* `"4 * 3 - 2 + 1"` should return:

  ```
  [((Infix '*' (Literal 4.0) (Infix '-' (literal 3.0) (Infix '+' (Literal 2.0) (Literal 1.0)))),"")]
  ```

* `"4 * (3 - 2 + 1)"` should return:

  ```
  [((Infix '*' (Literal 4.0) (Paren (Infix '-' (literal 3.0) (Infix '+' (Literal 2.0) (Literal 1.0))))),"")]
  ```

### 3.11. Create a primitive parser `parseChar`, rewrite `parseInfix`
You may have written the `parseInfix` function above to inspect the
character at the front of the string according to whether that
character was an infix operator. Lets extract the logic of the portion
of the parser that inspects the character into it's own function. Call
this function `parseChar`, it should have the type:

``` haskell
parseChar :: (Char -> Bool) -> ReadS Char
```

This function should take a predicate `(Char -> Bool)` and if the
character at the head of the input string matches the predicate, that
character should be removed from the output string and returned. If
the predicate does not match the head of the input string, return an
empty list.

Finally, rewrite `parseInfix` to use your new `parseChar` function. Be
sure to test your function with the same input and the same output as
in exercise 3.10.

### 3.12. Making use of the stack to implement precedence and associativity
As we learned in exercise 3.10, our simple `infixParser` does not
honor the standard operator precedence or associativity (unless, of
course, you figured out how to parse with precedence and associativity
before arriving at this exercise).

For example, our parser will always parse infix operators expressions,
regardless of the standard mathematical order of operations. So
`5 + 4 - 3 * 2 / 1` will parse the same way as `5 / 4 * 3 - 2 + 1`,
will parse the same way as, `5 + 4 + 3 + 2 + 1`:

* 5 + (4 - (3 * (2 / 1)))
* 5 / (4 * (3 - (2 + 1)))
* 5 + (4 + (3 + (2 + 1)))

The way the parentheses bunch-up towards the right is called "right
associativity." Some operators, such as the power operator is right
associative (2^2^2 is equal to 2^(2^2)). However all other infix
operators are left associative, so we need to correct our parser.

What we want is to follow the standard mathematical order of
operations:

* `5 + 4 - 3 * 2 / 1` = `(5 + 4) - ((3 * 2) / 1)`
* `5 / 4 * 3 - 2 + 1` = `(((5 / 4) * 3) - 2) + 1`
* `5 + 4 + 3 + 2 + 1` = `(((5 + 4) + 3) + 2) + 1`

First lets focus on associativity, and then lets focus on operator
precedence (a.k.a. "order of operations").

#### 3.12.1. Implement an left-associative infix parser
To implement left-associativity, we will write a recursive parser with
a stack. In Haskell, since list data structures are lazy, we can use
them as a stack. So let's modify our `parseInfix` function to take a
list of `CalcAST` values as an argument, this will serve as our stack.

``` haskell
parseInfix :: [CalcAST] -> ReadS CalcAST
```

Lets hand-parse a simple example: `1 + 2 + 3`, which should parse to
an expression equivalent to:

``` haskell
Infix '+' (Infix '+' (Literal 1.0) (Literal 2.0)) (Literal 3.0)
```

Here are the parse steps that should happen in order:

1. `inStr = "1+2+3", stack = []`

   `parseInfix` calls `parseCalc` which parses `'1'` from the input,
   pushes it to the stack.
   
2. `inStr = "+2+3", stack = [Literal 1.0]`

    `parseInfix` parses `'+'` from the input, keeps this `'+'`
    character in a local variable which we will call `opcode`.

3. `inStr = "2+3", stack = [Literal 1.0]`

    `parseInfix` calls `parseCalc` which parses `'2'` from the input,
    pushes it to the stack.

4. `inStr = "+3", stack = [Literal 1.0, Literal 2.0]`

    `parseInfix` pops the last two elements off of the stack, and uses
    the `opcode` local variable to construct an expression
    `Infix '+' (Literal 1.0) (Literal 2.0)`. This expression is then
    pushed onto the stack.

5. `inStr = "+3", stack = [Infix '+' (Literal 1.0) (Literal 2.0)]`

    Now `parseInfix` function checks if `inStr` is empty. If it is
    empty, return the top of the stack, otherwise recurse to step 2.

Test the new `parseInfix` function with the input string
`"1 + 2 + 3 + 4"`, you should get the result:

``` haskell
Infix '+' (Infix '+' (Infix '+' (Literal 1.0) (Literal 2.0)) (Literal 3.0)) (Literal 4.0)
```

#### 3.12.2. A stack from function composition `(.)` and partial function application
In exercise 3.12.1 we use a list as our stack data structure. But a
chain of function compositions can also act as a stack data
structure. Take this expression as an example:

``` haskell
let add = (+) :: Int -> Int -> Int
let mul = (*) :: Int -> Int -> Int
let stack = (mul 1) . (mul 2). (add 3) . (add 4) . (add 5) :: (Int -> Int)
(stack 0) -- execute stack with initial value 0
```

In the above example, we define functions `add` and `mul` which both
take two `Int` arguments. When we define `stack`, we chain a series of
`add` or `mul` expressions together. Each `add` or `mul` expression
contains only 1 argument, so the Haskell runtim will allocate a lambda
data structure that would look something like this: `(\ x -> 5 +
x)`.

Notice that the lambda contains both essential pieces of information:
the `5` opreand and the `+` operator. So the lambda here is acting as
a sort of opaque tuple data structure (a tuple on which you cannot do
pattern matching). Our stack is formed by the composition operator
`(.)` which itself is a lambda containing it's left and right-hand
function arguments in it's own tuple-like structure.

Haskellers have a name for this technique of using partial function
application to store information in lambdas, and to use a chain of
function composition operators as a stack: it is is called an
**"endofunctor"**, named after a similar concept from the mathematics
of category theory.

Let's create a type synonym `Endo a`:

``` haskell
type Endo a = (a -> a)
```

The data type `Endo` has the very important property that if
`f :: Endo a` and `g :: Endo a` then `(f . g) :: Endo a`, and also
`(g . f) :: Endo a`. The function `id` is the "empty" endofunctor,
analogous to the empty list `[]`.

So the integer arithmetic stack in the above example `mul 1 . mul 2
. add 3 . add 4 . add 5` would be of type `Endo Int`.

Are there any data types in our program that we could use as a type of
`Endo`? What about the `Infix` constructor? **YES!** We can do a
partial function application with a character value `Char`, for
example `('+' :: Char)` and another `CalcAST` expression, for example
`(Literal 1.0)`, and partially applied this to the first two fields of
the `Infix` data structure, resulting in a type of:

``` Haskell
let stack = (Infix '+' (Literal 1.0)) :: Endo CalcAST
```

In exercise 3.12.1 we rewrote our `parseInfix` function to take a list
of `CalcAST` values as an argument. Lets rewrite `parseInfix` again,
this time taking an `Endo CalcAST` as an argument instead of a
list. Build and run the parser with the same test that we used for
exercise 3.12.1.

**HINT:** when you test your remember to use `id` instead of an empty
list to run the function.

#### 3.12.3. Respecting the operator precedence (order of operations)
The final change we need to make to our `parseInfix` function is for
it to respect the order of operations, that is to say, the
multiplication operator needs to bind "more stronglyq" than the addition
operator. To do this, we simply include one more argument to the
`parseInfix` function -- an argument `prec` of type `Int`:

``` haskell
oarseInfix :: Endo CalcAST -> Int -> ReadS CalcAST
```

We then change the logic of `parseInfix` to check the precedence of an
operator before parsing the next operand. The logic is fairly simple:

* a parser greedily parses as many operators as it can as long as each
  next operator has the same precedence as the one before it.

* If the next operand has a higher prescedence than the current
  operator, a `parseInfix` is called with a higher precedence level
  and a new stack containing only the current operand and
  operator. The result of this recursive call is then treated as a
  single operand and placed back onto the stack.

* If the next operand has a lower prescedence than the current
  operator, the parser should place the current operand onto the
  stack, then return the stack. This will end the current call to
  `parseInfix` for that precedence level, and return to the function
  context of the lower precedence level which called it.

Here is an illustration. Imagine we are parsing the string
`"1+2+3*4*5+6+7-8"`, what would the calling context (local variables)
of the `parseInfix` function look like at each step of the parse.

1. Our initial function call to `parseInfix` has this context:

   ``` haskell
   prec=0
   inStr="1+2+3*4*5+6+7-8"
   stack=id
   ```

   First we parse `"1"` and store it in a local variable `lhs`
   ("left-hand side"). Then we try to parse an infix operator. If
   there is no infix operator, we simply return the value of
   `lhs`. But since the `'+'` operators is next and it has a higher
   precedence than 0, so we call `parseInfix` with a new stack
   containing the value of `lhs` and the value of `'+'`.

2. Now the context of `parseInfix` is this

   ``` haskell
   prec=1
   inStr="1+2+3*4*5+6+7-8"
   stack=(Infix '+' (Literal 1))
   ```

   `parseInfix` will parse until the first multiplication sign, the
   first operator with a precedence not equal to 1.

3. We have a `lhs` value and an operator with a higher prescedence
   than what we had before:

   ``` haskell
   prec=1
   inStr="4*5+6+7-8"
   stack=(Infix '+' (Infix '+' (Literal 1.0)) (Literal 2.0))
   lhs=Literal 3
   opcode='*'
   opcodPrec=2 -- is greater than prec=1
   ```

   Now we need to make a recursive call to `parseInfix` with a new
   stack and a new precedence value. But we will return to this
   calling context when all the multiplication signs have been parsed.

4. This begins the `parseInfix` function that was called by the calling
   context in step 3. The execution context of this function is:

   ``` haskell
   prec=2
   inStr="4*5+6+7-8"
   stack=(Infix '*' (Literal 3.0))
   ```

   We will continue this way until we get to the next `'+'`
   character, the first operator in the remaining input with a
   prescedence not equal to 2.

5. All of the multiplication operators have been parsed. We now need
   to return the value on the stack:

   ``` haskell
   prec=2
   inStr="4*5+6+7-8"
   stack=(Infix '*' (Infix '*' (Literal 3.0) (Literal 4.0)) (Literal 5.0))
   ```

   The value of the `stack` is returned and will become the value
   stored in the `lhs` local variable.

6. We are now back in the function context from step 3 above, but
   after the recursive call to `parseInfix`.

   ``` haskell
   prec=1
   inStr="4*5+6+7-8"
   stack=(Infix '+' (Infix '+' (Literal 1.0)) (Literal 2.0))
   lhs=(Infix '*' (Infix '*' (Literal 3.0) (Literal 4.0)) (Literal 5.0))
   opcode='+'
   opcodPrec=1
   ```

7. Parse infix now parses to the end of the string and returns the value on the stack.

   ``` haskell
   prec=1
   inStr=""
   stack=(Infix '-' (Infix '+' (Infix '+' (Infix '+' (Infix '+' (Infix '+' (Literal 1.0)) (Literal 2.0)) (Infix '*' (Infix '*' (Literal 3.0) (Literal 4.0)) (Literal 5.0))) (Literal 6.0)) (Literal 7.0)) (Literal 8.0))
   ```

8. We are now at the top-most calling context with the `prec` value at
   zero. There are no more operators on the `inStr`, so the next call
   to `parseChar` returns an empty list, so we return the value of
   `lhs` as it is.

See if you can rewrite `parseInfix` to behave like that.

### 3.13. Use the parser and the evaluator together.
We now have three necessary components for a programming language:

1. an AST, which we defined in exercise 1,
2. an evaluator, which we defined in exercise 2, and
3. a parser, which we defined in exercise 3.

Although most computer programming languages have many more than just
3 components to them (e.g. linting, macro expansion, type checking,
optimization, instrumentation, object code emission), all computer
programming languages have, at the very least, these three components
that we have just built in the preceeding exercises.

The AST is common to both the evaluator and the parser, so the final
step in building our miniature programming languge is to take the
output of the parser function `parseCalc`, and feed it as input to the
evaluator function `calcEval`. Create a new function `calcEvalString`
and use it to parse and evaluate an input string. The type should be:

``` haskell
calcEvalString :: String -> Evaluate Double
```

This function should return `Left` with an error message if the parser
did not parse all of the input string.

### 3.14. Writing a general testing function
Lets update our `main` function in `Calculator.hs` to include parsing
tests. All the parsing tests that were given in exercise section 3 are
written below.

The `main` program will be changed such that now, when you
`Calculator` from the command line, if the tests pass, the program
will pause and wait for you to input a string. The string will be
evaluated by `calcEvalString`, allowing you to test your program with
any input you like. Type "quit" or "exit" to stop the program.

``` haskell
-- This function uses code that we defined in exercise 2.5. Write this
-- just above the 'main' function.

parseTest :: (Eq a, Show a) => ReadS a -> [(String, [(a, String)])] -> IO ()
parseTest = testCase

-- | This function enters into an REPL for your calculator program.
calcREPL :: IO ()
calcREPL = do
    putStr "calc> "
    inStr <- dropWhile isSpace `fmap` getLine
    (   if      inStr == "quit" || inStr == "exit" then return ()
        else if inStr == "" then calcREPL -- (Quietly skip empty lines.)
        else do
            case calcEvalString line of
                Right result -> putStrLn (show result)
                Left  err    -> putStrLn ("(Error: " ++ err ++ ")")
            calcREPL
    )

-- This new 'main' function is similar to the old. You can delete the old
-- 'main' and replace it with the one here.

main :: IO ()
main = do
    let ok = return
    let nope = Left ""
    calcTest
        [ (Literal 1.618, ok 1.618)
        , (Literal 0, ok 0)
        , (Label "pi", ok pi)
        , (Label "e", ok (exp 1.0))
        , (Label "Nami's Cosmological Constant", nope)
        , (Paren (Literal 1.618), ok 1.618)
        , (Paren (Label "FooBar"), nope)
        , (Paren (Paren (Literal 1.618)), ok 1.618)
        , (Paren (Paren (Paren (Label "FooBar"))), nope)
        , (Infix '+' (Literal 1) (Literal 2), ok 3.0)
        , (Infix '+' (Literal 5) (Infix '*' (Literal 2) (Literal 3)), ok 11.0)
        , (Infix '*' (Paren (Infix '+' (Literal 5) (Literal 2))) (Literal 3), ok 21.0)
        , (Infix '/' (Infix '*' (Literal 2) (Label "pi")) (Literal 2), ok pi)
        , (Infix '+' (Infix '&' (Literal 0) (Literal 1)) (Literal 2), nope)
        , (Function "sqrt" (Literal 2), ok (sqrt 2))
        , (Function "sin" (Paren (Infix '*' (Literal 2) (Label "pi"))), ok (sin (2*pi)))
        , (Paren (Function "log" (Paren (Function "exp" (Literal 1)))), ok (log (exp 1.0)))
        ]
    -- If any of the tests above failed, the program will have halted
    -- with 'fail' on that test, as the behavior is defined on line
    -- 82. If all tests passed, this final line of code is executed.
    putStrLn "All evaluator tests passed."
    ---------------
    let ok = return
    let nope = []
    parseTest dropWS
        [ ("    hello", ok ((), "hello"))
        , ("hello    ", ok ((), "hello    "))
        ]
    parseTest parseLabel
        [ ("pi * 2", ok (Label "pi", " * 2"))
        , ("2 * pi", nope)
        , ("  pi", nope)
        ]
    parseTest parseLiteral
        [ ("1618 * 2", ok (Literal 1618.0," * 2"))
        , ("-1618 * 2", ok (Literal (-1618.0)," * 2"))
        , ("1.618 * 2", ok (Literal 1.618," * 2"))
        , ("-1.618 * 2", ok (Literal (-1.618)," * 2"))
        , ("1618e3 * 2", ok (Literal 1618000.0," * 2"))
        , ("1.618e3 * 2", ok (Literal 1618.0," * 2"))
        , ("1618e-3 * 2", ok (Literal 1.618," * 2"))
        , (" -1.618e3 * 2", nope)
        ]
    parseTest (parseChoice parseLabel parseLiteral)
        [ ("abc 123", ok (Label "abc", " 123"))
        , ("123 abc", ok (Literal 123.0, " abc"))
        , (".123 abc", nope)
        , ("  123 abc", nope)
        ]
    parseTest parseParen
        [ ("(abc)", ok (Paren (Label "abc"),""))
        , ("(123)", ok (Paren (Literal 123.0), ""))
        , ("(.123)", nope)
        , (" 123)", nope)
        , ("(123", nope)
        ]
    parseTest parseCalc
        [ ("((abc))", ok (Paren (Paren (Label "abc")),""))
        , ("((123))", ok (Paren (Paren (Literal 123.0)),""))
        , ("((123)))", ok (Paren (Paren (Literal 123.0)),")"))
        , ("(((123))", nope)
        , ( "1 + 2 + 3 * 4 * 5 + 6 + 7 - 8"
          , ok (Infix '-'
                 ( Infix '+'
                   ( Infix '+'
                     ( Infix '+'
                       ( Infix '+'
                         ( Infix '+' (Literal 1.0)) (Literal 2.0)
                       )
                       (Infix '*' (Infix '*' (Literal 3.0) (Literal 4.0)) (Literal 5.0))
                     )
                     (Literal 6.0))
                   (Literal 7.0))
                 (Literal 8.0)
               )
           )
        , ( "(1 + 2) * 3"
          , ok (Infix '*' (Paren (Infix '+' (Literal 1.0) (Literal 2.0))) (Literal 3.0))
          )
        , ( "1 + (2 * 3)"
          , ok (Infix '+' (Literal 1.0) (Paren (Infix '*' (Literal 2.0) (Literal 3.0))))
          )
        ]
    putStrLn "All parser tests passed."
    ---------------
    calcREPL
```

## 4. Handling parser errors
Using `ReadS` is a good way to throw together a quick parser, but it
doesn't tell us very much about what went wrong. Haskell provides a
function `Prelude.error` for throwing an exception, however this will
bring your entire program to a crashing halt.

A more gentle way of reporting errors is to wrap function results in
an `Either` data type, with `Left` indicating an error condition, and
`Right` indicating success. Although `Either` is useful for all sorts
of things, using it to indicate success or failure is a very common
practice in Haskell.

The `Either` data type is both a Monad so you can use it with `do`
notation. It is also an Applicative functor, so you can use `fmap` to
change the value of the `Right` constructor, and you can use the apply
operator (`<*>`) to apply functions of type `Parser` to pure
functions.

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
`ReadS` as an parameter which we will convert. Let's call the function
the "parameter function", because it is taken as a parameter (a.k.a
argument) to the function.

The `liftReadS` function should behave like so:

* If the `ReadS` parameter function returns an empty list, `liftReadS`
  should return a `Left` containing the `ErrorMessage`.

* If the `ReadS` parameter function returns a list that contains
  **exactly one** result tuple, return that result tuple in a `Right`
  constructor.

* If the `ReadS` parameter function returns multiple results, return a
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

parseTest :: Eq a => Parser a -> [(String, Either (a, String))] -> IO ()
parseTest = testCase
    -- Only the type of this function changes.
```

The `parseLiteral` function should evaluate to a `Left` error message
when given an unmatching or empty string, the error message should be
`"expecting a literal"`.

#### Recommendation:
I strongly recomend you **only** change the types of these functions
at first without rewriting any other part of your code, and then try
to recompile. The `ghc` compiler will produce a long list of compiler
error messages. Use these error messages to your advantage; these
errors are Haskell's way telling you how to change your program
safely.

#### Hint: use `liftReadS` to convert functions that use `reads`
Don't forget to use `liftReadS` that we defined in exercise 4.2 in
every function where the `Prelude.reads` function is used. You may use
`liftReadS` to convert all of your functions, but `parseLabel` and can
probably easily be re-written without `liftReadS`. You will not be
able to use `liftReadS` to convert `parseCalc` or `parseChoice`.

## 4.4. Write `parse1Char` and `parseManyChars` primitive parsers.
Now that we have our own `Parser` type, lets break down some the
parsing operations we will be doing often to their most fundamental
"primitive" behaviors. We can then define other parsers in terms of
these primitives. Let's begin by defining 2 new primitive functions.

1. `parse1Char` must have a type of:

    ``` haskell
    parse1Char :: ErrorMessage -> (Char -> Bool) -> Parser Char
    ```

    This function takes a predicate function of type `(Char -> Bool)`
    and apply it to the first character in the input string. If the
    input string is empty, or if the predicate function evaluates to
    `False`, the `parse1Char` function must evaluate to a `Left` value
    containing the `ErrorMessage` string.

2. `parseManyChars` must have a type of:

    ``` haskell
    parseManyChars :: ErrorMessage -> (Char -> Bool) -> Parser String
    ```

    This function takes a predicate of type `(Char -> Bool)` and uses
    this predicate with the `Prelude.span` function on the input
    string. If the input string is empty, or if the `span` function
    matched zero characters, `parseManyChars` must evaluate to a
    `Left` value containin the `ErrorMessage` string.

## 4.5. Use `fmap` and `parseManyChars` to rewrite `parseLabel`
Compare the `parseLabel` function to the `parseManyChars`
function. Notice that they are essentially the same function, except
`parseManyChars` is parameterized over a predicate, whereas
`parseLabel` specifies the `Data.Char.isAlpha` predicate.

Rewrite `parseLabel` to use `parseManyChars` with the `isAlpha`
predicate, and use the `fmap` function to wrap the output of
`parseManyChars` in the `Label` data constructor. The error message
returned by `parseLabel` should be `"expecting a label"`.

## 4.6. Misleading error messages? Lets simplify our primitives.
Lets look at what happens when we run the `parseParen` function with
an empty string (run `parseParen ""` in GHCI to see what
happens). What error message occurs?

The error that occurs is probably "expecting a literal" (it could also
be "expecting a label"). Why do we get this error when we are
expecting a parenthetical expression?

Inspecting the `parseParen` function we see that it was defined using
the `parseChoice` function (which we first wrote in 3.4, and then
rewrote in 4.3). As it is defined, `parseChoice` will ignore the
`Left` values of the first parameter parser and always return the
`Left` value of the second parameter parser.

This could be lead to some very confusing and misleading error
messages. To correct the problem, we may be tempted to write a new
function like this:

``` haskell
parseLiteralOrLabel :: Parser CalcAST
parseLiteralOrLabel inStr =
    case parseChoice parseLiteral parseLabel inStr of
        Left{}  -> Left "expecting symbol or label"
        Right a -> Right a
```

But we would have to write a case statements every time we want to
rewrite the error message. Writing `if` and `case` statements all
throughout the program is the last thing a Haskeller wants to do.

Can we write a higher-order function so we can express the logic of
the parser in this in a more elegant way?

### 4.6.1. Define a function that evaluates to an empty error message
Create a function called `failParse` (the function `Prelude.fail` is a
Haskell standard function and should not be renamed). This function
should always evaluate to `Left` with an empty string as the error
message. The type of this function should be:

``` haskell
failParse :: Parser void
```

(**NOTE:** the type variable named `void` above has no special
meaning, it could also be defined as `failParse :: Parser a`. The use
of `void` as a variable name here simply indicates to other
programmers that the function will always fail.)

### 4.6.2. Define a function that rewrites an error message
Create a function called `expecting`. The type of this function should
be:

``` haskell
expecting :: ErrorMessage -> Parser any -> Parser any
```

This function takes an error message and a parser function as
parameters. The result of the parameter parser should be inspected with
a `case` statement. If the result is any `Left` value, `expecting`
should throw away the old `ErrorMessage` value and evaluate to a value
of `Left` containing the new `ErrorMessage` given as an argument to
`expecting`. The `ErrorMessaeg` value should be prepended by the string
`"expecting "`. If the result of the argument parser is `Right`,
`expecting` returns the `Right` value unchanged.

### 4.6.3. Rewrite the primitive functions to use `failParse`
We defined the primitive functions `parse1Char` and `parseManyChars`
in exercise 4.4, and the `liftReadS` function in exercise
4.2. Originally, the types of these functions were:

``` haskell
liftReadS :: ErrorMessage -> ReadS any -> Parser any

parse1Char :: ErrorMessage -> (Char -> Bool) -> Parser Char

parseManyChars :: ErrorMessage -> (Char -> Bool) -> Parser String
```

Change these functions so that they no longer take an `ErrorMessage`
parameter, and instead evaluate to `failParse` rather than throwing
the given `ErrorMessage`.

The new types of these functions should be:

``` haskell
liftReadS :: ReadS any -> Parser any

parse1Char :: (Char -> Bool) -> Parser Char

parseManyChars :: (Char -> Bool) -> Parser String
```

You will also need to rewrite the following functions to make use of
`expecting`, although their function types are unchanged from before.

``` haskell
parseLabel :: Parser CalcAST

parseLiteral :: Parser CalcAST

parseParen :: Parser CalcAST

parseCalc :: Parser CalcAST
```

## 4.7. Write the `parseFunc` function for parsing "sin", "cosh", "exp", and "log"

# 5. Turn `Parser` into our own custom monad
