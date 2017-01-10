module HuttonRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit value) = value
eval (Add lhs rhs) = eval lhs + eval rhs

printExpr :: Expr -> String
printExpr (Lit value) = show value
printExpr (Add lhs rhs) = printExpr lhs ++ " + " ++ printExpr rhs
