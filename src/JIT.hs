{-# LANGUAGE OverloadedStrings #-}

module JIT where

import Data.Word
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import LLVM.AST
import qualified LLVM.AST as AST

import LLVM.PassManager

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod = runExceptT $ do
    liftIO $ withContext $ \context ->
        withModuleFromAST context mod $ \m -> do
            withPassManager passes $ \pm -> do
                runPassManager pm m
                optmod <- moduleAST m
                s <- moduleLLVMAssembly m
                BS.putStrLn s
                return optmod
