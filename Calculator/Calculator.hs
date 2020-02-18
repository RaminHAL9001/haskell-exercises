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

main :: IO ()
main = return ()
