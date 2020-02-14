module Main where

import Data.Char

type CalcNumber = Double
type CalcLabel  = String
type CalcOpCode = Char

data CalcAST
  = Literal  CalcNumber
  | Label    CalcLabel
  | Infix    CalcOpCode CalcAST CalcAST
  | Function CalcLabel  CalcAST
  | Paren    CalcAST
  deriving (Eq, Read, Show)

type ErrorMessage = String

type Evaluate a = Either ErrorMessage a

getConstant :: CalcLabel -> Evaluate CalcNumber
getConstant lbl = case lbl of
  "pi" -> Right pi
  "e"  -> Right (exp 1.0)
  _    -> Left ("unknown constant " ++ show lbl)

getArithmetic :: CalcOpCode -> Either ErrorMessage (CalcNumber -> CalcNumber -> CalcNumber)
getArithmetic opcode = case opcode of
  '+' -> Right (+)
  '-' -> Right (-)
  '*' -> Right (*)
  '/' -> Right (/)
  _   -> Left ("unknown infix operator " ++ show opcode)

getFunction :: CalcLabel -> Evaluate (CalcNumber -> CalcNumber)
getFunction lbl = case lbl of
  "sin"  -> Right sin
  "cos"  -> Right cos
  "tan"  -> Right tan
  "sqrt" -> Right sqrt
  "exp"  -> Right exp
  "log"  -> Right log
  "asin" -> Right asin
  "acos" -> Right acos
  "atan" -> Right atan
  "sinh" -> Right sinh
  "cosh" -> Right cosh
  "tanh" -> Right tanh
  _      -> Left ("unknown function " ++ show lbl)

calcEval :: CalcAST -> Evaluate CalcNumber
calcEval expr = case expr of
  Literal         lit  -> Right lit
  Label           sym  -> getConstant sym
  Infix opcode  a  b   -> do
    oper <- getArithmetic opcode
    oper <$> calcEval a <*> calcEval b
  Function lbl    arg  -> do
    func <- getFunction lbl
    func <$> calcEval arg
  Paren           expr -> calcEval expr

--------------------------------------------------------------------------------
-- Parsers

dropWS :: String -> String
dropWS = dropWhile isSpace

parseLabel :: String -> (String, String)
parseLabel = span isAlpha

-- type ReadS a = String -> [(a, String)]
-- reads :: Read a => ReadS a

parseLiteral :: String -> [(CalcAST, String)]
parseLiteral inStr = case reads inStr of
  [(num, outStr)] -> [(Literal num, outStr)]
  _               -> []

--------------------------------------------------------------------------------
-- Tests

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

--------------------------------------------------------------------------------
-- Main

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
    putStrLn "All tests passed."
