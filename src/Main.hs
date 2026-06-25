{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
-- import Codegen
-- import Emit

import Control.Monad.Trans
import System.Console.Haskeline

import qualified LLVM.AST as AST

process :: String -> IO ()
process line = do
    let res = parseTopLevel line
    case res of
        Left err -> print err
        Right ex -> mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
    where
    loop = do
        minput <- getInputLine "ready> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> (liftIO $ process input) >> loop

-- process :: AST.Module -> String -> IO (Maybe AST.Module)
-- process modo source = do
--     let res = parseTopLevel source
--     case res of
--         Left err -> print err >> return Nothing
--         Right ex -> do
--             ast <- codegen modo ex
--             return $ Just ast
-- 
-- main :: IO ()
-- main = runInputT defaultSettings (loop initModule)
--     where
--     loop mod = do
--         minInput <- getInputLine "ready> "
--         case minInput of
--             Nothing -> outputStrLn "Goodbye."
--             Just input -> do
--                 modn <- liftIO $ process mod input
--                 case modn of
--                     Just modn -> loop modn
--                     Nothing -> loop mod
--                 -- (liftIO $ process input) >> loop
