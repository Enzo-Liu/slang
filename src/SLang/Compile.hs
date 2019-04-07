{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}

module SLang.Compile where

import           SLang.Expr

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.String
import           LLVM.AST                   hiding (function)
import           LLVM.AST.Type              as AST

import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T

import qualified LLVM.IRBuilder.Constant    as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR

data CodegenState = CodegenState {
  primFuncMap :: M.Map Name Operand,
  argMap      :: M.Map Name Operand
                                 }

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

emptyCodegen :: CodegenState
emptyCodegen = CodegenState M.empty M.empty

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

containsFunc :: Name -> CodeBuilder Bool
containsFunc n = gets $ M.member n . primFuncMap

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
  mapM_ compile' defs
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

toArgMap :: [Operand] -> M.Map Name Operand
toArgMap = M.fromList . map (\o@(LocalReference _ n)-> (n, o))

compileFunc :: [SLExpr] -> CodeBuilder Operand
-- function definition
compileFunc (expr : args')
  | isSymbolDef expr = do
      let (SLAtom (SLASymbol nameText):params:bodys) = args'
          name = mkName $ T.unpack nameText
          paramNames = map symbolName . subExprs $ params
          paramTypes = map (\n -> (i32, fromString $ T.unpack n)) paramNames
      containsFunc name >>= (`when` error "conflict function name")
      function name paramTypes i32 $ \ops -> do
        lastArgMap <- gets argMap
        modify (\s -> s {argMap = toArgMap ops})
        results <- mapM compile' bodys
        modify (\s -> s {argMap = lastArgMap})
        -- the last value as function returns
        IR.ret $ results !! (length results - 1)

-- call function
compileFunc (SLAtom (SLASymbol fname):args') = do
  ops <- mapM compile' args'
  compielFunc' fname ops
compileFunc _ = error "should not enter"

compileConstant :: SLAtom -> CodeBuilder Operand
compileConstant (SLAInt int)  = IR.int32 $ fromIntegral int
compileConstant (SLASymbol s) = (M.! (fromString $ T.unpack s) ) <$> gets argMap
compileConstant _             = undefined

compielFunc' :: T.Text -> [Operand] -> CodeBuilder Operand
compielFunc' "+" (op1:ops)       = foldM IR.add op1 ops
compielFunc' "-" [op1]           = IR.int32 0 >>= (`IR.sub` op1)
compielFunc' "-" (op1:ops)       = foldM IR.sub op1 ops
compielFunc' "*" (op1:ops)       = foldM IR.mul op1 ops
compielFunc' "/" (op1:ops@(_:_)) = foldM IR.udiv op1 ops
compielFunc' n ops                 = do
  let name = fromString $ T.unpack n
  hasFunc <- containsFunc name
  unless hasFunc (error . T.unpack $ n <> "not implimented")
  f <- getFunc name
  IR.call f (map (,[]) ops)
