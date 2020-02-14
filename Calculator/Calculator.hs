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

getConstant :: CalcLabel -> Either ErrorMessage CalcNumber
getConstant lbl = case lbl of
  "pi" -> Right pi
  "e"  -> Right (exp 1.0)
  _    -> Left ("unknown constant " ++ show lbl)

calcEval :: CalcAST -> Either ErrorMessage CalcNumber
calcEval expr = case expr of
  Literal         lit  -> Right lit
  Label           sym  -> getConstant sym

testExpressions :: [CalcAST]
testExpressions =
    [ Literal 1.618
    , Label "pi"
    , Label "Nami's Cosmological Constant"
    ]

main :: IO ()
main = mapM_ print
    (fmap (\ test -> (test, calcEval test)) testExpressions)
