module Main where

data CalcAST
  = Literal  Double
  | Label    String
  | Infix    Char CalcAST CalcAST
  | Function String CalcAST
  | Paren    CalcAST
  deriving (Eq, Read, Show)

main :: IO ()
main = return ()
