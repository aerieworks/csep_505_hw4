module TypeCheck (checkType, parseAndCheckStr, freeTypeVars, alphaRename, subst, parseStr, eraseStr, checkClosed) where

import Data.Char
import Data.List
import Expr
import Result
import SExp -- used only by parseAndCheckStr
import Token -- used only by parseAndCheckStr

-- A typing context includes a set of polymorphic type variables that
-- are in scope, along with bindings of program variables to their
-- declared types.
type TyContext = ([TVar], [(TVar, Type)])

-- Problem 2.
freeTypeVars :: Type -> [TVar] -> [TVar]
freeTypeVars ty bound = case ty of
  ArrowT t1 t2 -> (freeTypeVars t1 bound) ++ (freeTypeVars t2 bound)
  PairT t1 t2 -> (freeTypeVars t1 bound) ++ (freeTypeVars t2 bound)
  ListT t -> freeTypeVars t bound
  ForAllT v t -> freeTypeVars t (v:bound)
  VarT v -> if v `elem` bound then [] else v:[]
  otherwise -> []

-- Problem 3.
alphaRename :: TVar -> TVar -> Type -> Type
alphaRename vIn vOut ty = case ty of
  ArrowT t1 t2 -> (ArrowT (alphaRename vIn vOut t1) (alphaRename vIn vOut t2))
  PairT t1 t2 -> (PairT (alphaRename vIn vOut t1) (alphaRename vIn vOut t2))
  ListT t -> (ListT (alphaRename vIn vOut t))
  ForAllT v t -> (ForAllT (if v == vIn then vOut else v) (alphaRename vIn vOut t))
  VarT v -> (VarT (if v == vIn then vOut else v))
  otherwise -> ty

-- Implementation complete: nothing to do here. Use this helper in checkType.
checkClosed :: Type -> [TVar] -> Result ()
checkClosed ty bound =
  case freeTypeVars ty bound of
   [] -> Ok ()
   nonEmpty -> Err ("unbound type var(s) in " ++ (show ty) ++ ": " ++ (show nonEmpty))

-- Problem 4.
subst :: TVar -> Type -> Type -> Type
subst var forType inType =
  let capturableTVars = freeTypeVars forType []
      allInTVars = allTypeVars inType
  in substInner var forType inType capturableTVars (capturableTVars ++ allInTVars)

substInner :: TVar -> Type -> Type -> [TVar] -> [TVar] -> Type
substInner var forType inType capturableTVars existingTVars = case inType of
  VarT v -> (if v == var then forType else inType)
  ForAllT v t -> if v == var then inType else
    case v `elem` capturableTVars of
      True ->
        let v' = (genFreshVar existingTVars)
        in (substInner var forType (alphaRename v v' inType) capturableTVars (v':existingTVars))
      False -> ForAllT v (substInner var forType t capturableTVars existingTVars)
  ArrowT t1 t2 -> (ArrowT
    (substInner var forType t1 capturableTVars existingTVars)
    (substInner var forType t2 capturableTVars existingTVars))
  PairT t1 t2 -> (PairT
    (substInner var forType t1 capturableTVars existingTVars)
    (substInner var forType t2 capturableTVars existingTVars))
  ListT t -> (ListT (substInner var forType t capturableTVars existingTVars))
  otherwise -> inType

-- Problem 5.
checkType :: DExpr -> TyContext -> Result Type
checkType expr gamma =
  do t <- checkTypeInner expr gamma
     _ <- checkClosed t (fst gamma)
     return t

checkTypeInner :: DExpr -> TyContext -> Result Type
checkTypeInner expr gamma = case expr of
  NumD _ -> Ok NumT
  StringD _ -> Ok StringT
  IfD condExpr consExpr altExpr -> case checkTypeInner condExpr gamma of
    Ok BoolT ->
      do consType <- checkTypeInner consExpr gamma
         altType  <- checkTypeInner altExpr gamma
         if consType == altType
           then Ok consType
           else Err ("Type mismatch between if expression condition and alternate: " ++
             (show consType) ++ " != " ++ (show altType))
    Err msg -> Err msg
    Ok t -> Err ("If expression expects boolean condition, but found " ++ (show t))
  VarD var -> case lookup var (snd gamma) of
    Just t -> Ok t
    otherwise -> Err ("Untyped variable `" ++ (show var) ++ "`")
  FunD var inType body -> checkTypeApplicable var inType body gamma
  AppD appExpr argExpr ->
    do argType <- checkTypeInner argExpr gamma
       appType <- checkTypeInner appExpr gamma
       case appType of
         ArrowT inType outType -> if inType == argType
           then Ok outType
           else Err ("Function requires " ++ (show inType) ++ " but was called with " ++
             (show argType))
         otherwise -> Err ("Application expects applicable, but found " ++ (show appType))
  WithD var argExpr bodyExpr ->
    do inType <- checkTypeInner argExpr gamma
       ArrowT _ outType <- checkTypeApplicable var inType bodyExpr gamma
       Ok outType
  ForAllD tVar bodyExpr ->
    do bodyType <- checkTypeInner bodyExpr gamma
       Ok (ForAllT tVar bodyType)
  SpecD expr specType ->
    do exprType <- checkTypeInner expr gamma
       case exprType of
         ForAllT tVar bodyType -> Ok (subst tVar specType bodyType)
         otherwise ->
           Err ("Instantiation expects a polymorphic type, but found " ++ (show exprType))

checkTypeApplicable :: Var -> Type -> DExpr -> TyContext -> Result Type
checkTypeApplicable var inType body gamma =
    do outType <- checkTypeInner body (fst gamma, (var, inType):(snd gamma))
       Ok (ArrowT inType outType)

-- Implementation complete.
-- Generates a variable name that's distinct from every name in the argument list.
-- Use this helper for alpha-renaming in subst.
genFreshVar :: [String] -> String
genFreshVar [] = "a"
genFreshVar tabu = reverse (inc (reverse (maximum tabu)))
  where inc ('z':cs) = 'a':'z':cs
        inc (c:cs) = (succ c):cs

-- Implementation complete: nothing to do here. Use this helper for alpha-renaming in subst.
allTypeVars :: Type -> [TVar]
allTypeVars (ArrowT t1 t2) = (allTypeVars t1) ++ (allTypeVars t2)
allTypeVars (ListT t1) = allTypeVars t1
allTypeVars (PairT t1 t2) = (allTypeVars t1) ++ (allTypeVars t2)
allTypeVars (VarT v) = [v]
allTypeVars (ForAllT v t) = [v] ++ (allTypeVars t)
allTypeVars NumT = []
allTypeVars BoolT = []
allTypeVars StringT = []

initialTypeEnv :: [(Var, Type)]
initialTypeEnv = [
  ("true", BoolT),
  ("false", BoolT),
  ("+", ArrowT NumT (ArrowT NumT NumT)),
  ("*", ArrowT NumT (ArrowT NumT NumT)),
  ("=", ArrowT NumT (ArrowT NumT BoolT)),
  ("<", ArrowT NumT (ArrowT NumT BoolT)),
  ("cons", ForAllT "a" (ArrowT (VarT "a")
                        (ArrowT (ListT (VarT "a")) (ListT (VarT "a"))))),
  ("empty", ForAllT "a" (ListT (VarT "a"))),
  ("first", ForAllT "a" (ArrowT (ListT (VarT "a")) (VarT "a"))),
  ("rest", ForAllT "a" (ArrowT (ListT (VarT "a")) (ListT (VarT "a")))),
  ("empty?", ForAllT "a" (ArrowT (ListT (VarT "a")) BoolT)),
  ("cons?", ForAllT "a" (ArrowT (ListT (VarT "a")) BoolT)),
  ("pair", ForAllT "a" (ForAllT "b" (ArrowT (VarT "a") (ArrowT (VarT "b")
                                                        (PairT (VarT "a") (VarT "b")))))),
  ("fst", ForAllT "a" (ForAllT "b" (ArrowT (PairT (VarT "a") (VarT "b"))
                                    (VarT "a")))),
  ("snd", ForAllT "a" (ForAllT "b" (ArrowT (PairT (VarT "a") (VarT "b"))
                                    (VarT "b")))),
  ("fix", ForAllT "a" (ArrowT (ArrowT (VarT "a") (VarT "a")) (VarT "a"))),
  ("call/cc", ForAllT "a" (ArrowT (ArrowT (ArrowT (VarT "a") (ForAllT "b" (VarT "b")))
                                   (VarT "a"))
                           (VarT "a")))
  ]

parseAndCheckStr :: String -> Result Type
parseAndCheckStr str =
  let toks = tokenize str in
  do (sexp, _) <- parseSExp toks
     expr <- parseExpr sexp
     cexp <- desugar expr
     checkType cexp ([], initialTypeEnv)

parseStr :: String -> Result DExpr
parseStr str =
  let toks = tokenize str in
  do (sexp, _) <- parseSExp toks
     expr <- parseExpr sexp
     desugar expr

eraseStr :: String -> Result CExpr
eraseStr str =
  do dExpr <- parseStr str
     _     <- checkType dExpr ([], initialTypeEnv)
     return (erase dExpr)
