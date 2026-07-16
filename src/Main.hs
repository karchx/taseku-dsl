{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Codegen
import Emit

import Control.Monad.Trans
import System.Environment
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

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
    where
    loop modu = do
        minInput <- getInputLine "ready> "
        case minInput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
                result <- liftIO $ process modu input
                case result of
                    Just modn -> loop modn
                    Nothing -> loop modu

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [fname] -> processFile fname >> return ()
        _ -> putStrLn "Error: unknow"
