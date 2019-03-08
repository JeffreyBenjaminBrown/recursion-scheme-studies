{-# LANGUAGE DeriveFunctor #-}

module Expr where


--data Lit
--  = StrLit String
--  | IntLit Int
--  | Ident String
--  deriving (Show, Eq)

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

--data Stmt
--  = Break
--  | Continue
--  | Empty
--  | IfElse Expr [Stmt] [Stmt]
--  | Return (Maybe Expr)
--  | While Expr [Stmt]
--  | Expression Expr
--  deriving (Show, Eq)
