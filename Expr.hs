module Expr (Var, Expr(..), CExpr(..), DExpr(..), Type(..), TVar, parseExpr, desugar, erase, alphaEquiv) where

import Data.List
import Result
import SExp

type Var = String
type TVar = String

-- Types. Concrete syntax:
-- <t> ::= num
--       | bool
--       | (<t> -> <t>)
--       | (<t> , <t>)
--       | (list <t>)
--       | (forall a . <t>)
--       | a
data Type = NumT
          | BoolT
          | StringT
          | ArrowT Type Type
          | PairT Type Type
          | ListT Type
          | ForAllT TVar Type
          | VarT TVar

sndLookup :: Eq b => b -> [(a, b)] -> Maybe a
sndLookup b list = case list of
  [] -> Nothing
  (x, y):rest -> if y == b then Just x else sndLookup b rest

-- alphaEquiv determines whether two types are equivalent up to alpha
-- renaming of type variables.
-- Problem 1.
alphaEquiv :: Type -> Type -> [(TVar, TVar)] -> Bool
alphaEquiv type1 type2 typeVariableMap = case (type1, type2) of
  (NumT, NumT) -> True
  (BoolT, BoolT) -> True
  (StringT, StringT) -> True
  (ArrowT t1In t1Out, ArrowT t2In t2Out) -> (alphaEquiv t1In t2In typeVariableMap) && (alphaEquiv t1Out t2Out typeVariableMap)
  (PairT t1Left t1Right, PairT t2Left t2Right) -> (alphaEquiv t1Left t2Left typeVariableMap) && (alphaEquiv t1Right t2Right typeVariableMap)
  (ListT t1, ListT t2) -> alphaEquiv t1 t2 typeVariableMap
  (ForAllT v1 t1, ForAllT v2 t2) -> alphaEquiv t1 t2 ((v1, v2):typeVariableMap)
  (VarT v1, VarT v2) -> case (lookup v1 typeVariableMap, sndLookup v2 typeVariableMap) of
    (Just x, Just y) -> x == v2 && y == v1 -- Alpha-renaming
    (Nothing, Nothing) -> v1 == v2         -- Free variable types match.
  otherwise -> False

instance Eq Type where
  t1 == t2 = alphaEquiv t1 t2 []

instance Show Type where
  show NumT = "num"
  show BoolT = "bool"
  show StringT = "string"
  show (ArrowT t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"
  show (ListT t) = "(list " ++ (show t) ++ ")"
  show (PairT t1 t2) = "(" ++ (show t1) ++ ", " ++ (show t2) ++ ")"
  show (ForAllT v t) = "(forall " ++ v ++ "." ++ (show t) ++ ")"
  show (VarT v) = v

-- Expression syntax:
-- <e> ::= <number>
--       | "<string>"
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x ...) <e>)
--       | (<e> <e> ...)
--       | (with* ([x <e>] ...) <e>)
--       | (forall (t ...) <e>)
--       | (<> <e> <t> ...)
data Expr = NumE Integer
          | StringE String
          | IfE Expr Expr Expr
          | VarE Var
          | FunE [(Var, Type)] Expr
          | AppE [Expr]
          | WithStarE [(Var, Expr)] Expr
          | ForAllE [Var] Expr
          | SpecE Expr [Type]
          deriving (Eq, Show)

-- Desugared language (from Expr). This is the language we type-check.
-- <e> ::= <number>
--       | "<string>"
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x) <e>)
--       | (<e> <e>)
--       | (with [x <e>] <e>)
--       | (forall (t) <e>)
--       | (<> <e> <t>)
data DExpr = NumD Integer
           | StringD String
           | IfD DExpr DExpr DExpr
           | VarD Var
           | FunD Var Type DExpr
           | AppD DExpr DExpr
           | WithD Var DExpr DExpr
           | ForAllD Var DExpr
           | SpecD DExpr Type
           deriving (Eq, Show)

-- Core language (types erased from DExpr after checking, let desugared to app+fun).
-- <e> ::= <number>
--       | "<string>"
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x) <e>)
--       | (<e> <e>)
data CExpr = NumC Integer
           | StringC String
           | IfC CExpr CExpr CExpr
           | VarC Var
           | FunC Var CExpr
           | AppC CExpr CExpr
           deriving (Eq, Show)

parseType :: SExp -> Result Type
parseType (IdS "num") = return NumT
parseType (IdS "bool") = return BoolT
parseType (IdS "string") = return StringT
parseType (IdS v) = return (VarT v)
parseType (ListS [IdS "list", t]) =
  do t' <- parseType t
     return (ListT t')
parseType (ListS [t1, IdS ",", t2]) =
  do t1' <- parseType t1
     t2' <- parseType t2
     return (PairT t1' t2')
parseType (ListS [arg, IdS "->", res]) =
  do argT <- parseType arg
     resT <- parseType res
     return (ArrowT argT resT)
parseType (ListS [IdS "forall", IdS var, IdS ".", ty]) =
  do ty' <- parseType ty
     return (ForAllT var ty')
parseType bad = Err ("bad type expression: " ++ (show bad))

parseTypeParamList [] = Err "type parameter list not terminated with '>'"
parseTypeParamList [IdS ">"] = return []
parseTypeParamList (t:ts) =
  do ty <- parseType t
     tys <- parseTypeParamList ts
     return (ty:tys)

parseExpr :: SExp -> Result Expr
parseExpr (NumS n) = return (NumE n)
parseExpr (IdS id) = return (VarE id)
parseExpr (StringS s) = return (StringE s)
parseExpr (ListS ((IdS "if"):sexps)) =
  case mapM parseExpr sexps of
   Ok [cond, cons, alt] -> return (IfE cond cons alt)
   _ -> fail ("bad `if` subexprs: " ++ (show sexps))
parseExpr (ListS [IdS "fun", ListS vars, body]) =
  do body' <- parseExpr body
     vars' <- mapM parseFunVarAndType vars
     checkUnique vars'
     return (FunE vars' body')
parseExpr (ListS [IdS "with*", ListS bindings, body]) =
  do body' <- parseExpr body
     bindings' <- mapM parseBinding bindings
     return (WithStarE bindings' body')
parseExpr (ListS [IdS "forall", ListS tvars, body]) =
  do body' <- parseExpr body
     tvars' <- mapM parseTVar tvars
     checkUnique tvars'
     return (ForAllE tvars' body')
parseExpr (ListS (expr:(IdS "<"):tys)) =
  do expr' <- parseExpr expr
     tys' <- parseTypeParamList tys
     return (SpecE expr' tys')
parseExpr (ListS sexps) =
  do exprs <- mapM parseExpr sexps
     return (AppE exprs)

parseTVar (IdS v) = return v
parseTVar nonId = fail ("expected type variable, got: " ++ (show nonId))

parseFunVarAndType (ListS [IdS v, IdS ":", ty]) =
  do ty' <- parseType ty
     return (v, ty')
parseFunVarAndType nonVarType = fail ("expected (var : type), got: " ++
                                        (show nonVarType))

checkUnique vars | vars == (nub vars) = Ok ()
                 | otherwise = Err ("duplicate var in: " ++ (show vars))

parseBinding (ListS [IdS var, bound]) =
  do bound' <- parseExpr bound
     return (var, bound')
parseBinding bad = fail ("expected var-expr binding, got: " ++ (show bad))

desugar :: Expr -> Result DExpr
desugar (NumE n) = return (NumD n)
desugar (VarE v) = return (VarD v)
desugar (StringE s) = return (StringD s)
desugar (IfE cond cons alt) =
  do cond' <- desugar cond
     cons' <- desugar cons
     alt' <- desugar alt
     return (IfD cond' cons' alt')
desugar (FunE vars body) =
  case vars of
   [] -> fail "no-arg function"
   [(var, ty)] ->
     do body' <- desugar body
        return (FunD var ty body')
   ((var, ty):vars) ->
     do fun' <- desugar (FunE vars body)
        return (FunD var ty fun')
desugar (WithStarE bindings body) =
  case bindings of
   [] -> desugar body
   ((var, expr):bindings') ->
     do expr' <- desugar expr
        body' <- desugar (WithStarE bindings' body)
        return (WithD var expr' body')
desugar (AppE exprs) =
  case exprs of
   [] -> fail "empty app"
   [expr] -> fail ("application with only one sub-expr: " ++ (show expr))
   [fun, arg] ->
     do fun' <- desugar fun
        arg' <- desugar arg
        return (AppD fun' arg')
   (fun:arg:args) -> desugar (AppE ((AppE [fun, arg]):args))
desugar (ForAllE vars expr) =
  case vars of
   [] -> desugar expr
   [v] -> do
     expr' <- desugar expr
     return (ForAllD v expr')
   (v:vs) -> do
     expr' <- desugar (ForAllE vs expr)
     return (ForAllD v expr')
desugar (SpecE expr tys) =
  case tys of
   [] -> desugar expr
   [ty] -> do
     expr' <- desugar expr
     return (SpecD expr' ty)
   (ty:tys) -> desugar (SpecE (SpecE expr [ty]) tys)

-- Problem 6.
erase :: DExpr -> CExpr
erase dExpr = case dExpr of
  NumD v -> NumC v
  StringD v -> StringC v
  IfD dCond dCons dAlt -> IfC (erase dCond) (erase dCons) (erase dAlt)
  VarD v -> VarC v
  FunD v _ dBody -> FunC v (erase dBody)
  AppD dApp dArg -> AppC (erase dApp) (erase dArg)
  WithD v dArg dBody -> AppC (FunC v (erase dBody)) (erase dArg)
  ForAllD _ expr -> erase expr
  SpecD expr _ -> erase expr

