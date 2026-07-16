{-# LANGUAGE OverloadedStrings #-}

module JIT where

import Data.Word
import qualified Data.ByteString.Char8 as BS
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.ExecutionEngine as EE

import LLVM.PassManager

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
    where
        optlevel = Just 0
        model    = Nothing
        ptrelim  = Nothing
        fastins  = Nothing

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod = runExceptT $ do
    liftIO $ withContext $ \context ->
        jit context $ \executionEngine ->
            withModuleFromAST context mod $ \m -> do
                withPassManager passes $ \pm -> do
                    optmod <- moduleAST m
                    s <- moduleLLVMAssembly m
                    BS.putStrLn s

                    EE.withModuleInEngine executionEngine m $ \ee -> do
                        mainfn <- EE.getFunction ee (AST.Name "main")
                        case mainfn of
                            Just fn -> do
                                res <- run fn
                                putStrLn $ "Evaluated to: " ++ show res
                            Nothing -> return ()

                    return optmod
