{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}

module SLang.Compile where

import           SLang.Expr

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           LLVM.AST hiding (function)
import           LLVM.AST.Type              as AST

import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T

import qualified LLVM.IRBuilder.Constant    as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR

data CodegenState = CodegenState {
  primFuncMap :: M.Map Name Operand
                                 }

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

emptyCodegen :: CodegenState
emptyCodegen = CodegenState M.empty

execCodegen :: Codegen Module -> Module
execCodegen m = evalState (runCodegen m) emptyCodegen

type CodeBuilder a = IR.IRBuilderT (IR.ModuleBuilderT Codegen) a

execCodeBuilder :: CodeBuilder a -> Module
execCodeBuilder cb =
  execCodegen $ IR.buildModuleT "slang.ll" (IR.execIRBuilderT IR.emptyIRBuilder cb)

extern :: Name -> [Type] -> Type -> CodeBuilder Operand
extern n tys rt = lift (IR.extern n tys rt) >>= instPrim n

function :: Name -> [(Type, IR.ParameterName)] -> Type -> ([Operand] -> CodeBuilder ()) -> CodeBuilder Operand
function n tys rt insts = lift (IR.function n tys rt insts) >>= instPrim n

instPrim :: Name -> Operand -> CodeBuilder Operand
instPrim n op = do
  modify $ \s -> s{primFuncMap = M.insert n op (primFuncMap s)}
  return op

getFunc :: Name -> CodeBuilder Operand
getFunc n = gets $ (M.! n) . primFuncMap

putInt32Name :: Name
putInt32Name = "putInt32"

printfName :: Name
printfName = "printf"

compile :: SLProgram -> Module
compile (SLProgram exprs) = execCodeBuilder $ mdo
  _ <- extern printfName [ptr i8, i32] AST.void
  _ <- function putInt32Name [(i32, "a")] AST.void $ \ops ->
    getFunc printfName >>= (`putsInt` head ops)
  let defs = filter isDef exprs
      instructions = filter (not . isDef) exprs
  function "main" [] i32 $ \_ -> do
    mapM_ (compile' Control.Monad.>=>
             (\op -> getFunc putInt32Name >>= (`IR.call` [(op, [])])))
      instructions
    ret <- IR.int32 0
    IR.ret ret

compile' :: SLExpr -> CodeBuilder Operand
compile' (SLExpr exprs) = compileFunc exprs
compile' (SLAtom atom)  = compileConstant atom

putsInt :: Operand -> Operand -> CodeBuilder ()
putsInt printf op = do
  -- "%d" = 37,100 = 00100101,01100100 = 9572 = 37 << 8 + 100
  intFormat <- IR.globalStringPtr "%d\n" "intFormat"
  _ <- IR.call printf [(intFormat, []), (op,[])]
  IR.retVoid

compileFunc :: [SLExpr] -> CodeBuilder Operand
compileFunc (SLAtom (SLASymbol fname):args') = do
  ops <- mapM compile' args'
  compielFunc' fname ops
compileFunc _ = error "should not enter"

compileConstant :: Applicative m => SLAtom -> m Operand
compileConstant (SLAInt int) = IR.int32 $ fromIntegral int
compileConstant _            = undefined

compielFunc' ::IR.MonadIRBuilder m => T.Text -> [Operand] -> m Operand
compielFunc' "+" (op1:ops)       = foldM IR.add op1 ops
compielFunc' "-" [op1]           = IR.int32 0 >>= (`IR.sub` op1)
compielFunc' "-" (op1:ops)       = foldM IR.sub op1 ops
compielFunc' "*" (op1:ops)       = foldM IR.mul op1 ops
compielFunc' "/" (op1:ops@(_:_)) = foldM IR.udiv op1 ops
compielFunc' _ _                 = error "not implemented"
