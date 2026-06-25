{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Codegen
import Emit

import Control.Monad.Trans
import System.Console.Haskeline

import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
    let res = parseTopLevel source
    case res of
        Left err -> print err >> return Nothing
        Right ex -> do
            ast <- codegen modo ex
            return $ Just ast

main :: IO ()
main = runInputT defaultSettings (loop initModule)
    where
    loop modu = do
        minInput <- getInputLine "ready> "
        case minInput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
                modn <- liftIO $ process modu input
                case modn of
                    Just modn -> loop modn
                    Nothing -> loop modu
