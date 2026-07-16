module AST (Expr(..)) where

type Name = String

data Expr
    = Int Integer
    | Float Double
    | BinOp Name Expr Expr
    | Var String
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    | UnaryOp Name Expr
    | If Expr Expr Expr
    | For Name Expr Expr Expr Expr
    | Let Name Expr Expr
    deriving (Eq, Ord, Show)
