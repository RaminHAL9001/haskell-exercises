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

calcEval :: CalcAST -> Evaluate CalcNumber
calcEval expr = case expr of
  Literal         lit  -> Right lit
  Label           sym  -> getConstant sym
  Infix opcode  a  b   -> do
    oper <- getArithmetic opcode
    a    <- calcEval a
    b    <- calcEval b
    Right (oper a b)

testExpressions :: [CalcAST]
testExpressions =
    [ Literal 1.618
    , Label "pi"
    , Label "Nami's Cosmological Constant"
    ]

main :: IO ()
main = mapM_ print
    (fmap (\ test -> (test, calcEval test)) testExpressions)
