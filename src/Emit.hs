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

getArgName :: S.Expr -> String
getArgName (S.Var n) = n
getArgName _ = error "The invalid argument"

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name exprArgs body) = do
    define double (fromString name) fnargs bls
    where
        strArgs = map getArgName exprArgs

        fnargs = toSig (map fromString strArgs)

        bls = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM strArgs $ \a -> do
                var <- alloca double
                store var (local (AST.Name (fromString a)))
                assign a var
            cgen body >>= ret

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
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen(S.Var x) = getVar x >>= load
cgen(S.Call fn args) = do
    largs <- mapM cgen args
    let argTypes = replicate (length args) double
    call (externf (AST.Name (fromString fn)) argTypes) largs
cgen (S.BinOp  op a b) = do
    case Map.lookup op binops of
        Just f -> do
            ca <- cgen a
            cb <- cgen b
            f ca cb
        Nothing -> error "No such operator"

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

