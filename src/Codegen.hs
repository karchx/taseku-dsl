{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen 
    ( Codegen(..)
    , LLVM (..)
    , getVar
    , emptyModule
    , runLLVM
    , createBlocks
    , entryBlockName
    , execCodegen
    , setBlock
    , addBlock
    , alloca
    , define
    , double
    , external
    , store
    , local
    , assign
    , fadd
    , fsub
    , fmul
    , fdiv
    , fcmp
    , cons
    , uitofp
    , call
    , load
    , emptyBlock
    , ret
    , toSBS
    , fromSBS
    , externf) where

import Data.List
import Data.Word (Word32)
import Data.Function
import qualified Data.Map as Map
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as B8

import Control.Monad.State
import Control.Applicative

import LLVM.AST.AddrSpace (AddrSpace(..))
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as F
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type (Type(FloatingPointType, PointerType), FloatingPointType(DoubleFP))


------------------------------------------------------------------------------------------
-- Utils 
------------------------------------------------------------------------------------------

toSBS :: String -> SBS.ShortByteString
toSBS = SBS.toShort . B8.pack

fromSBS :: SBS.ShortByteString -> String
fromSBS = B8.unpack . SBS.fromShort

------------------------------------------------------------------------------------------
-- Module Level
------------------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: SBS.ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> SBS.ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = body
    }

external :: Type -> SBS.ShortByteString -> [(Type, Name)] -> LLVM ()
external retty  label argtys = addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }

type SymbolTable = [(String, Operand)]

data CodegenState
    = CodegenState {
       currentBlock :: Name
     , blocks       :: Map.Map Name BlockState
     , symtab       :: SymbolTable
     , blockCount   :: Int
     , count        :: Word
     , names        :: Names
    } deriving Show

data BlockState
    = BlockState {
       idx   :: Int
     , stack :: [Named Instruction]
     , term  :: Maybe (Named Terminator)
    } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
    where
        maketerm (Just x) = x
        maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: SBS.ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

double :: Type
double = FloatingPointType DoubleFP

entry :: Codegen Name
entry = gets currentBlock

addBlock :: SBS.ShortByteString -> Codegen Name
addBlock bname = do
    bls <- gets blocks
    ix <- gets blockCount
    nms <- gets names

    let new = emptyBlock ix
        bnameStr = fromSBS bname
        (qname, supply) = uniqueName bnameStr nms
        qnameSBS = toSBS qname

    modify $ \s -> s { blocks = Map.insert (Name qnameSBS) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }
    return (Name qnameSBS)

setBlock :: Name -> Codegen Name
setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show c

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
    case Map.lookup nm ns of
        Nothing -> (nm, Map.insert nm 1 ns)
        Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

local :: Name -> Operand
local = LocalReference double

externf :: Name -> [Type] -> Operand
externf name argTypes =
    let funcType = FunctionType double argTypes False
        ptrType  = PointerType funcType (AddrSpace 0)
    in ConstantOperand $ C.GlobalReference ptrType name

assign :: String -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symtab
    modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getVar :: String -> Codegen Operand
getVar var = do
    syms <- gets symtab
    case lookup var syms of
        Just x -> return x
        Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Instruction -> Codegen Operand
instr ins = do
    n <- fresh
    blk <- current
    let i = stack blk
    let ref = (UnName n)
    modifyBlock $ blk { stack = i ++ [ref := ins] }
    return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
    blk <- current
    modifyBlock $ blk { term = Just trm }
    return trm

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

fcmp :: F.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = do
    n <- fresh
    let ref = UnName n
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = i ++ [ref := Alloca ty Nothing 0 []] } )
    return $ LocalReference (PointerType ty (AddrSpace 0)) ref

store :: Operand -> Operand -> Codegen ()
store ptr val = do
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = i ++ [Do $ Store False ptr val Nothing 0 []] })

load :: Operand -> Codegen Operand 
load ptr = instr $ Load False ptr Nothing 0 []


-- Control flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

