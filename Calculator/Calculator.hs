module Main where

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

calcEval :: CalcAST -> Either ErrorMessage CalcNumber
calcEval expr = case expr of
  Literal         lit  -> Right lit
  Label           "pi" -> Right pi
  Label           sym  -> Left ("unknown symbol " ++ show sym)

testExpressions :: [CalcAST]
testExpressions =
    [ Literal 1.618
    , Label "pi"
    , Label "Nami's Cosmological Constant"
    ]

main :: IO ()
main = mapM_ print
    (fmap (\ test -> (test, calcEval test)) testExpressions)
