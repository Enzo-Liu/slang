{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TupleSections              #-}

module SLang.Compile where

import           SLang.Expr

import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Short      as BSS
import           Data.Maybe
import           Data.String
import           LLVM.AST                   hiding (function)
import           LLVM.AST.Type              as AST

import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T

import qualified LLVM.AST.IntegerPredicate  as IR
import qualified LLVM.IRBuilder.Constant    as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR

-- to suppport inscrement build, this should contains all the info needed
data CodegenState = CodegenState {
  primFuncMap :: M.Map Name Operand,
  localArgMap :: M.Map Name Operand,
  globalArgMap :: M.Map Name (CodeBuilder Operand)
                                 }

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadState CodegenState )

emptyCodegen :: CodegenState
emptyCodegen = CodegenState M.empty M.empty M.empty

execCodegen :: Codegen Module -> CodegenState -> (Module, CodegenState)
execCodegen m = runState (runCodegen m)

type CodeBuilder a = IR.IRBuilderT (IR.ModuleBuilderT Codegen) a

execCodeBuilder :: CodeBuilder a -> CodegenState -> (Module, CodegenState)
execCodeBuilder cb =
  execCodegen $ IR.buildModuleT "slang.ll" (IR.execIRBuilderT IR.emptyIRBuilder cb)

extern :: Name -> [Type] -> Type -> CodeBuilder Operand
extern n tys rt = lift (IR.extern n tys rt) >>= instPrim n

externVarArgs :: Name -> [Type] -> Type -> CodeBuilder Operand
externVarArgs n tys rt = lift (IR.externVarArgs n tys rt) >>= instPrim n

function :: Name -> [(Type, IR.ParameterName)] -> Type -> ([Operand] -> CodeBuilder ()) -> CodeBuilder Operand
function n tys rt insts = mdo
  _ <- instPrim n op
  op <- lift (IR.function n tys rt insts)
  return op

instPrim :: Name -> Operand -> CodeBuilder Operand
instPrim n op = do
  modify $ \s -> s{primFuncMap = M.insert n op (primFuncMap s)}
  return op

instGlobal :: Name -> CodeBuilder Operand -> CodeBuilder Operand
instGlobal n op = do
  modify $ \s -> s{globalArgMap = M.insert n op (globalArgMap s)}
  op

getFunc :: Name -> CodeBuilder Operand
getFunc n = do
  fv <- funcValue
  gv <- globalValue
  return . head . catMaybes $ [fv, gv, error "no binded value"]
  where
    funcValue :: CodeBuilder (Maybe Operand)
    funcValue = gets $ M.lookup n . primFuncMap
    globalValue :: CodeBuilder (Maybe Operand)
    globalValue = do
     g <- gets $ M.lookup n . globalArgMap
     case g of
       Nothing -> return Nothing
       Just b  -> Just <$> b

containsFunc :: Name -> CodeBuilder Bool
containsFunc n = do
  isFunc <- gets $ M.member n . primFuncMap
  isLambda <- gets $ M.member n . globalArgMap
  return $ isFunc || isLambda

putInt32Name :: Name
putInt32Name = "putInt32"

printfName :: Name
printfName = "printf"

block :: BSS.ShortByteString -> CodeBuilder Name
block = (IR.block `IR.named`)

initModule :: (Module, CodegenState)
initModule = flip execCodeBuilder emptyCodegen $ do
  _ <- externVarArgs printfName [ptr i8] AST.i32
  function putInt32Name [(i32, "a")] AST.i32 $ \ops ->
    putsInt (head ops) >>= IR.ret

compileWithState :: SLProgram -> CodegenState -> (Module, CodegenState)
compileWithState (SLProgram exprs ) codegenState = flip execCodeBuilder codegenState $ mdo
  let defs = filter isDefun exprs
      instructions = filter (not . isDefun) exprs
  mapM_ compile' defs
  function "main" [] i32 $ \_ -> do
    _ <- block "main-entry"
    mapM_ compile' instructions
    ret <- IR.int32 0
    IR.ret ret


compile :: SLProgram -> (Module, CodegenState)
compile prog = (newMod, result)
  where (initMod, initState) = initModule
        (complied, result) = compileWithState prog initState
        newMod  = initMod {moduleDefinitions =
                           moduleDefinitions initMod ++
                           moduleDefinitions complied
                          }
compileLambda :: [Operand] -> [SLExpr] -> CodeBuilder ()
compileLambda ops body = do
    lastArgMap <- gets localArgMap
    modify (\s -> s {localArgMap = toArgMap ops})
    results <- mapM compile' body
    modify (\s -> s {localArgMap = lastArgMap})
    -- the last value as function returns
    IR.ret $ results !! (length results - 1)

compile' :: SLExpr -> CodeBuilder Operand
compile' (SLLambda args' body) = do
  let paramTypes = map (\n -> (i32, fromString $ T.unpack n)) args'
  name <- IR.freshName "lambda"
  function name paramTypes i32 (`compileLambda` body)

compile' (SLDefine sym inst) = instGlobal n (compile' inst)
  where n = mkName $ T.unpack sym
compile' (SLFunction f args' body) = do
  let name = mkName $ T.unpack f
      paramTypes = map (\n -> (i32, fromString $ T.unpack n)) args'
  containsFunc name >>= (`when` error "conflict function name")
  function name paramTypes i32 (`compileLambda` body)

compile' (SLSymbol s) = do
  lv <- localValue
  gv <- globalValue
  return . head . catMaybes $ [lv, gv, error "no binded value"]
  where
    name = fromString $ T.unpack s
    localValue :: CodeBuilder (Maybe Operand)
    localValue = gets $ M.lookup name . localArgMap
    globalValue :: CodeBuilder (Maybe Operand)
    globalValue = do
     g <- gets $ M.lookup name . globalArgMap
     case g of
       Nothing -> return Nothing
       Just b  -> Just <$> b

compile' (SLIf flagExpr thenBody elseBody)  = mdo
  IR.br entry
  entry <- block "if-entry"
  flag <- compile' flagExpr
  true <- IR.bit 1
  branch <- IR.icmp IR.EQ flag true
  IR.condBr branch thenBlock elseBlock

  thenBlock <- block "if-then"
  thenRet <- compile' thenBody
  thenBlock' <- IR.currentBlock
  IR.br exitBlock

  elseBlock <- block "if-else"
  elseRet <- compile' elseBody  -- this may change the current block
  elseBlock' <- IR.currentBlock
  IR.br exitBlock

  exitBlock <- block "if-exit"
  IR.phi [(thenRet, thenBlock'), (elseRet, elseBlock')]

compile' (SLBool True)  = IR.bit 1
compile' (SLBool False)  = IR.bit 0

compile' (SLCall fn params)  = do
  ops <- mapM compile' params
  compileCall' fn ops

compile' (SLInt i)  = IR.int32 $ fromIntegral i
-- string to i8 vector
compile' (SLString s)  = IR.freshName "const-str" >>= IR.globalStringPtr (T.unpack s)

putsInt :: Operand -> CodeBuilder Operand
putsInt op = do
  printf <- getFunc printfName
  -- "%d" = 37,100 = 00100101,01100100 = 9572 = 37 << 8 + 100
  intFormat <- IR.globalStringPtr "%d\n" "intFormat"
  IR.call printf [(intFormat, []), (op,[])]

print' :: Operand -> CodeBuilder Operand
print' op = do
  printf <- getFunc printfName
  format <- IR.globalStringPtr "%s\n" "strFormat"
  IR.call printf [(format, []), (op,[])]

toArgMap :: [Operand] -> M.Map Name Operand
toArgMap = M.fromList . map (\o@(LocalReference _ n)-> (n, o))

compileCall' :: T.Text -> [Operand] -> CodeBuilder Operand
compileCall' "+" (op1:ops)       = foldM IR.add op1 ops
compileCall' "-" [op1]           = IR.int32 0 >>= (`IR.sub` op1)
compileCall' "-" (op1:ops)       = foldM IR.sub op1 ops
compileCall' "=" [op1,op2]       = IR.icmp IR.EQ op1 op2
compileCall' "*" (op1:ops)       = foldM IR.mul op1 ops
compileCall' "/" (op1:ops@(_:_)) = foldM IR.udiv op1 ops
compileCall' "or" (op1:ops)      = foldM IR.or op1 ops
compileCall' "print" [op1]       = print' op1
compileCall' n ops                 = do
  let name = fromString $ T.unpack n
  hasFunc <- containsFunc name
  unless hasFunc (error . T.unpack $ n <> " not implimented")
  f <- getFunc name
  IR.call f (map (,[]) ops)
