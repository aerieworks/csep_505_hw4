module InterpCont where

import Expr
import Result
import SExp
import Token
import TypeCheck

-- Values resulting from interpreting an expression.
data Val = NumV Integer -- a numeric constant
         | BoolV Bool -- a boolean constant
         | StringV String -- a string constant
         | EmptyV -- an empty list
         | ConsV Val Val -- a non-empty list
         | PairV Val Val -- a pair
         | FunV Var CExpr Env -- a function closure
         | PrimV String (Val -> Cont -> Result Val)  -- primitive: name and implementation

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (StringV s) = show s
  show EmptyV = "empty"
  show (ConsV h t) = "(cons " ++ (show h) ++ " " ++ (show t) ++ ")"
  show (PairV f s) = "(" ++ (show f) ++ ", " ++ (show s) ++ ")"
  show (FunV var body _) = "(fun (" ++ var ++ ") " ++ (show body) ++ ")"
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"

data Cont = DoneK
          | IfK CExpr CExpr Env Cont
          | AppK Val Env Cont
          | ArgK CExpr Env Cont
          | HandleK Val Cont
          | ContextK Val Cont
          deriving Show

handleError :: Cont -> Val -> Result Val
handleError k val = case k of
  IfK _ _ _ outerK -> handleError outerK val
  AppK _ _ outerK -> handleError outerK val
  ArgK _ _ outerK -> handleError outerK val
  ContextK _ outerK -> handleError outerK val
  HandleK handler outerK -> apply handler val outerK
  DoneK -> Err ("unhandled error: " ++ (show val))

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 k -> callK k (PrimV ("partial:" ++ name)
                        (\arg2 k ->
                          case (arg1, arg2) of
                           (NumV lv, NumV rv) -> callK k (op lv rv)
                           nonNum -> handleError k (StringV (name ++ " applied to: " ++
                                                             (show nonNum))))))

add = wrapBinaryArithOp "+" (\x y -> (NumV (x+y)))
mult = wrapBinaryArithOp "*" (\x y -> (NumV (x*y)))
equal = wrapBinaryArithOp "=" (\x y -> (BoolV (x == y)))
less = wrapBinaryArithOp "<" (\x y -> (BoolV (x < y)))

unimplemented name = PrimV name (\v k -> Err (name ++ ": unimplemented"))

cons = PrimV "cons"
  (\v k -> callK k (PrimV "partial:cons"
    (\list k ->
      case list of
        ConsV _ _ -> callK k (ConsV v list)
        EmptyV -> callK k (ConsV v EmptyV)
        otherwise -> Err ("cons applied to: " ++ (show list)))))
consP = PrimV "cons?"
  (\v k -> callK k (
    case v of
      ConsV _ _ -> BoolV True
      otherwise -> BoolV False))
emptyP = PrimV "empty?"
  (\v k -> callK k (
    case v of
      EmptyV -> BoolV True
      otherwise -> BoolV False))
first = PrimV "first"
  (\v k -> case v of
    ConsV h t -> callK k h
    otherwise -> Err ("first applied to: " ++ (show v)))
rest = PrimV "rest"
  (\v k -> case v of
    ConsV h t -> callK k t
    otherwise -> Err ("rest applied to: " ++ (show v)))

pair = PrimV "pair"
  (\f k -> callK k (PrimV "partial:pair"
    (\s k -> callK k (PairV f s))))
pairFst = PrimV "fst"
  (\p k -> case p of
    PairV f s -> callK k f
    otherwise -> Err ("fst applied to: " ++ (show p)))
pairSnd = PrimV "snd"
  (\p k -> case p of
    PairV f s -> callK k s
    otherwise -> Err ("snd applied to: " ++ (show p)))

raise = PrimV "raise" (\err k -> handleError k err)
callWithHandler = PrimV "call-with-handler"
  (\thunk k -> callK k (PrimV "partial:call-with-handler"
    (\handler k -> apply thunk (BoolV True) (HandleK handler k))))

callWithContext = PrimV "call-with-context"
  (\ctx k -> callK k (PrimV "partial:call-with-context"
    (\thunk k -> apply thunk (BoolV True) (ContextK ctx k))))
getContext = PrimV "get-context"
  (\_ k -> callK k (buildContextList k))

callCc = PrimV "call/cc"
  (\thunk k -> apply thunk (PrimV "call/cc continuation" (\arg k' -> callK k arg)) k)

bind prim@(PrimV name fn) = (name, prim)
bind nonPrim = error ("cannot bind " ++ (show nonPrim))

buildContextList :: Cont -> Val
buildContextList k =
  case k of
    DoneK -> EmptyV
    IfK _ _ _ outerK -> buildContextList outerK
    AppK _ _ outerK -> buildContextList outerK
    ArgK _ _ outerK -> buildContextList outerK
    HandleK _ outerK -> buildContextList outerK
    ContextK ctx outerK -> ConsV ctx (buildContextList outerK)

-- Populate initialEnv ...
initialEnv :: Env
initialEnv = [
  ("true", BoolV True),
  ("false", BoolV False),
  ("empty", EmptyV)] ++
  (map bind [add, mult, equal, less, emptyP, first, rest, cons,
             consP, pair, pairFst, pairSnd, callCc, callWithContext,
             getContext, callWithHandler, raise])

interp :: CExpr -> Env -> Cont -> Result Val
interp expr env k =
  case expr of
   NumC n -> callK k (NumV n)
   StringC s -> callK k (StringV s)
   FunC var body -> callK k (FunV var body env)
   VarC v ->
     case lookup v env of
      Nothing -> Err ("unbound id: " ++ v)
      Just val -> callK k val
   IfC cond cons alt -> interp cond env (IfK cons alt env k)
   AppC fun arg -> interp fun env (ArgK arg env k)

callK :: Cont -> Val -> Result Val
callK k val =
  case k of
   DoneK -> Ok val
   IfK cons alt env k ->
     case val of
      BoolV True -> interp cons env k
      BoolV False -> interp alt env k
      nonBool -> Err ("`if` expected bool, got: " ++ (show nonBool))
   ArgK arg env k -> interp arg env (AppK val env k)
   AppK fv env k -> apply fv val k
   HandleK _ k -> callK k val
   ContextK _ k -> callK k val

apply :: Val -> Val -> Cont -> Result Val
apply fv val k =
  case fv of
    FunV var body closEnv -> interp body ((var, val):closEnv) k
    PrimV _ impl -> impl val k
    otherwise -> Err ("Application expects applicable, got: " ++ (show fv))

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr =
  let recur = checkIds bound reserved in
  case expr of
   NumC _ -> return ()
   StringC _ -> return ()
   IfC cond cons alt ->
     do recur cond
        recur cons
        recur alt
   VarC v | not (v `elem` bound) -> Err ("unbound id: " ++ v)
          | otherwise -> return ()
   FunC var body | var `elem` reserved -> Err ("cannot rebind reserved id: " ++ var)
                 | otherwise -> checkIds (var:bound) reserved body
   AppC fun arg ->
     do recur fun
        recur arg

interpStr :: String -> Result Val
interpStr str =
  let initialIds = map fst initialEnv in
  do expr <- eraseStr str
     _    <- checkIds initialIds (["fun", "if", "with*"] ++ initialIds) expr
     interp expr initialEnv DoneK

runFile :: String -> IO ()
runFile filename =
  do input <- readFile filename
     putStr (show (interpStr input) ++ "\n")
