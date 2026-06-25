module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import AST

binary s assoc = Ex.Infix (reservedOp s >> return (BinOp s)) assoc

table = [[binary "*"  Ex.AssocLeft,
          binary "/"  Ex.AssocLeft]
        ,[binary "+"  Ex.AssocLeft,
          binary "-"  Ex.AssocLeft]]

int :: Parser Expr
int = do
    n <- integer
    return $ Float (fromInteger n)

floating :: Parser Expr
floating = do
    n <- float
    return $ Float n

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = do
    var <- identifier
    return $ Var var

function :: Parser Expr
function = do
    reserved "func"
    name <- identifier
    args <- parens $ many variable
    body <- expr
    return $ Function name args body

extern :: Parser Expr
extern = do
    reserved "extern"
    name <- identifier
    args <- parens $ many variable
    return $ Extern name args

call :: Parser Expr
call = do
    name <- identifier
    args <- parens $ commaSep expr
    return $ Call name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> (parens expr)

fn :: Parser Expr
fn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

topLevel :: Parser [Expr]
topLevel = many $ do
    func <- fn
    reservedOp ";"
    return func

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s = parse (contents topLevel) "<stdin>" s

