{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Context
import LLVM.Module
import LLVM.AST
import LLVM.AST.Global

import Data.String (fromString)
import Control.Monad.Except
import Control.Applicative
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as BS
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified Data.Map as Map

import Codegen
import JIT
import qualified AST as S

zero = cons $ C.Float (F.Double 0.0)
false = zero

getArgName :: S.Expr -> String
getArgName (S.Var n) = n
getArgName _ = error "The invalid argument"

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
    define double (fromString name) fnargs bls
    where
        fnargs = toSig (map fromString args)
        bls = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \a -> do
                var <- alloca double
                store var (local (AST.Name (fromString a)))
                assign a var
            cgen body >>= ret

codegenTop (S.UnaryFunc name args body) =
    codegenTop $ S.Function ("unary" ++ name) args body

codegenTop (S.BinaryFunc name args body) =
    codegenTop $ S.Function ("binary" ++ name) args body

codegenTop (S.Extern name exprArgs) = do
    external double (fromString name) fnargs
    where
        strArgs = map getArgName exprArgs
        fnargs = toSig (map fromString strArgs)

codegenTop exp = do
    define double "main" [] blks
    where
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            cgen exp >>= ret

toSig :: [SBS.ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.BinOp  op a b) = do
    case Map.lookup op binops of
        Just f -> do
            ca <- cgen a
            cb <- cgen b
            f ca cb
        Nothing -> error "No such operator"
cgen (S.UnaryOp op a) = do
    cgen $ S.Call ("unary" ++ op) [a]
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen(S.Var x) = getVar x >>= load
cgen(S.Call fn args) = do
    largs <- mapM cgen args
    let argTypes = replicate (length args) double
    call (externf (AST.Name (fromString fn)) argTypes) largs
cgen (S.If cond tr fl) = do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    -- $entry
    -- ------------
    cond <- cgen cond
    test <- fcmp FP.ONE false cond
    cbr test ifthen ifelse

    -- if.then
    -- ------------
    setBlock ifthen
    trval <- cgen tr
    br ifexit
    ifthen <- getBlock

    -- if.else
    -- ------------
    setBlock ifelse
    flval <- cgen fl
    br ifexit
    ifelse <- getBlock

    -- if.exit
    -- ------------
    setBlock ifexit
    phi double [(trval, ifthen), (flval, ifelse)]

cgen (S.For ivar start cond step body) = do
    forloop <- addBlock "for.loop"
    forexit <- addBlock "for.exit"

    -- % entry
    -- --------------------------------------
    i <- alloca double
    istart <- cgen start
    stepval <- cgen step

    store i istart
    assign ivar i
    br forloop

    -- for.loop
    -- --------------------------------------
    setBlock forloop
    cgen body
    ival <- load i
    inext <- fadd ival stepval
    store i inext

    cond <- cgen cond
    test <- fcmp FP.ONE false cond
    cbr test forloop forexit

    -- for.exit
    -- --------------------------------------
    setBlock forexit
    return zero

cgen (S.BinOp op a b) = do
    case Map.lookup op binops of
        Just f -> do
            ca <- cgen a
            cb <- cgen b
            f ca cb
        Nothing -> cgen (S.Call ("binary" ++ op) [a, b])

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
    test <- fcmp FP.ULT a b
    uitofp double test

binops = Map.fromList [
          ("+", fadd)
        , ("-", fsub)
        , ("*", fmul)
        , ("/", fdiv)
        , ("<", lt)
    ]

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
    res <- runJIT oldast
    case res of
        Right newast -> return newast
        Left err     -> putStrLn err >> return oldast
    where
        modn   = mapM codegenTop fns
        oldast = runLLVM mod modn

