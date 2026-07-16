module AST (Expr(..)) where

type Name = String

data Expr
    = Int Integer
    | Float Double
    | BinOp Name Expr Expr
    | Var String
    | Call Name [Expr]
    | Function Name [Name] Expr
    | Extern Name [Expr]
    | UnaryOp Name Expr
    | If Expr Expr Expr
    | For Name Expr Expr Expr Expr
    | Let Name Expr Expr
    | BinaryFunc Name [Name] Expr
    | UnaryFunc Name [Name] Expr
    deriving (Eq, Ord, Show)
