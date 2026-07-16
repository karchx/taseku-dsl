module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import AST

int :: Parser Expr
int = do
    n <- integer
    return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

binary s assoc = Ex.Infix (reservedOp s >> return (BinOp s)) assoc

table = [[binary "*"  Ex.AssocLeft,
          binary "/"  Ex.AssocLeft]
        ,[binary "+"  Ex.AssocLeft,
          binary "-"  Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft]]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = Var <$> identifier

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

ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond <- expr
    reserved "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return $ If cond tr fl

for :: Parser Expr
for = do
    reserved "for"
    var <- identifier
    reservedOp "="
    start <- expr
    reservedOp ","
    cond <- expr
    reservedOp ","
    step <- expr
    reserved "in"
    body <- expr
    return $ For var start cond step body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> ifthen
      <|> for
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

