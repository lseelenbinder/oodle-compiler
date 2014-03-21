{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Oodle.TypeChecker (typeChecker, buildType) where

import Data.List (nub)

import Oodle.Error
import Oodle.ParseTree
import Oodle.SymbolTable

-- Check Types
--
type Scope = (SymbolTable, Declaration, Declaration)

typeChecker :: SymbolTable -> Start -> Error Bool
typeChecker st (Start classes) = do
  c <- mapM (tcC st) classes
  return $ and c

tcC :: SymbolTable -> Class -> Error Bool
tcC st (Class _ (Id name) _ vars methods) = do
  cd <- getClassDecl st name
  v <- mapM (tcV (st, cd, cd)) vars
  m <- mapM (tcM st cd) methods
  return $ and (v ++ m)

tcV :: Scope -> Var -> Error Bool
tcV (st, m, cls) (Var tk (Id i) t expr) = do
  exprT <- calculateTypeE (st, m, cls) expr
  if (t == exprT) || (t == TypeNull) || (exprT == TypeNoop) then
    return True
  else
    fail (msgWithToken tk (wrongTypeMsg t exprT) i)

tcM :: SymbolTable -> Declaration -> Method -> Error Bool
tcM st cls (Method _ (Id name) _ _ vars stmts) = do
  m <- getMethodDecl cls name
  v <- mapM (tcV (st, m, cls)) vars
  s <- mapM (tcS (st, m, cls)) stmts
  return $ and (v ++ s)

tcS :: Scope -> Statement -> Error Bool

tcS (st, m, cls) (AssignStatement tk (Id name) expr) = do
  tId <- getVarType m cls (Id name)
  t <- calculateTypeE (st, m, cls) expr
  if t == tId then
    return True
  else
    fail (msgWithToken' tk $ wrongTypeMsg tId t)

tcS (st, m, cls) (AssignStatement tk (IdArray name exprs) expr) = do
  tId <- getVarType m cls (IdArray name exprs)
  t <- calculateTypeE (st, m, cls) expr
  eT <- mapM (calculateTypeE (st, m, cls)) exprs
  let e = if null exprs then TypeInt else (head . nub) eT
  if t == tId then
    if TypeInt == e then return True
    else
      fail (msgWithToken' tk $ wrongTypeMsg TypeInt e)
  else fail $ msgWithToken' tk $ wrongTypeMsg tId t

tcS (st, m, cls) (IfStatement _ cond stmts stmts') =
  tcS' (st, m, cls) cond (stmts ++ stmts')

tcS (st, m, cls) (LoopStatement _ cond stmts) =
  tcS' (st, m, cls) cond stmts

tcS (st, m, cls) (CallStatement tk scope (Id name) args) = do
  scope' <- resolveScope (st, m, cls) scope
  method <- getMethodDecl scope' name
  actualP <- mapM (calculateTypeE (st, m, cls)) args
  let formalP = getParameterTypes method

  if length actualP == length formalP then
    if actualP == formalP then return True
    else
    fail (msgWithToken tk ("actual parameters do not match formal parameters ("
      ++ show actualP ++ " !=  "++ show formalP) name)
  else fail $ msgWithToken tk "incorrect number of parameters" name

tcS' :: Scope -> Expression -> [Statement] -> Error Bool
tcS' (st, m, cls) expr stmts = do
  e <- calculateTypeE (st, m, cls) expr
  s <- mapM (tcS (st, m, cls)) stmts
  if e == TypeBoolean then return $ and s
  else
    error $ msgWithToken' (getExprToken expr) "conditional not a boolean"
  where

calculateTypeE :: Scope -> Expression -> Error Type
calculateTypeE (st, m, cls) e =
  case e of
    ExpressionInt _ _               -> return TypeInt
    ExpressionId _ i                -> getVarType m cls i
    ExpressionStr _ _               -> return TypeString
    ExpressionTrue _                -> return TypeBoolean
    ExpressionFalse _               -> return TypeBoolean
    ExpressionMe _                  -> return $ TypeId (Id "me")
    ExpressionNew _ t               -> return t
    ExpressionNull _                -> return TypeNull
    ExpressionNoop                  -> return TypeNoop

    -- The only expression that needs secondary verification
    ExpressionCall tk scope (Id name) args -> do
      valid <- tcS (st, m, cls) (CallStatement tk scope (Id name) args)
      scp <- resolveScope (st, m, cls) scope
      method <- getMethodDecl scp name
      return $ if valid then getType method else TypeNoop

    ExpressionIdArray _ i           -> getVarType m cls i
    ExpressionNeg _ expr            -> checkTypeE' expr TypeInt
    ExpressionPos _ expr            -> checkTypeE' expr TypeInt
    ExpressionMul _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionDiv _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionAdd _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionSub _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionStrCat _ expr1 expr2  -> checkBothTypeE' expr1 expr2 TypeString
    ExpressionEq _ expr1 expr2      -> checkTypeArray expr1 expr2 [TypeString, TypeInt, TypeBoolean]
    ExpressionGt _ expr1 expr2      -> checkTypeArray expr1 expr2 [TypeString, TypeInt]
    ExpressionGtEq _ expr1 expr2    -> checkTypeArray expr1 expr2 [TypeString, TypeInt]
    ExpressionAnd _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeBoolean
    ExpressionOr _ expr1 expr2      -> checkBothTypeE' expr1 expr2 TypeBoolean
    ExpressionNot _ expr            -> checkTypeE' expr TypeBoolean
  where
    checkTypeE'         = checkTypeE (st, m, cls)
    checkBothTypeE'     = checkBothTypeE (st, m, cls)
    checkBothTypeMulE'  = checkBothTypeMulE (st, m, cls)
    checkTypeArray e1 e2 types = do
      t <- checkBothTypeMulE' e1 e2 types
      if t then return TypeBoolean else fail "OOPS"

checkTypeE :: Scope -> Expression -> Type -> Error Type
checkTypeE (st, m, cls) expr expectedT = do
  t <- calculateTypeE (st, m, cls) expr
  if t == expectedT then
    return t
  else
    fail $ msgWithToken' (getExprToken expr) $ wrongTypeMsg expectedT t

checkBothTypeE :: Scope -> Expression -> Expression -> Type -> Error Type
checkBothTypeE (st, m, cls) e1 e2 t = do
  t' <- checkTypeE' e1 t
  checkTypeE' e2 t'
  where checkTypeE' = checkTypeE (st, m, cls)

checkBothTypeMulE :: Scope -> Expression -> Expression -> [Type] -> Error Bool
checkBothTypeMulE _ expr _ [] = fail $
  msgWithToken' (getExprToken expr)
  "expression didn't match any of the expected types"
checkBothTypeMulE scope e1 e2 (t:types) = do
  e1' <- ctE e1
  e2' <- ctE e2

  child <- checkBothTypeMulE scope e1 e2 types
  return $ t == e1' && t == e2' || child
  where ctE = calculateTypeE scope

wrongTypeMsg :: Type -> Type -> String
wrongTypeMsg expectedT t =
  "expected type: " ++ show expectedT ++ " got type: " ++ show t

getVarType :: Declaration -> Declaration -> Id -> Error Type
getVarType m cls (Id name) = do
  d <- getNamedDecl (getVariables m ++ getVariables cls) name
  return $ type' d
getVarType m cls (IdArray name exprs) = do
  t <- getVarType m cls (Id name)
  return $ unbuildArray t (length exprs)

resolveScope :: Scope -> Expression -> Error Declaration
resolveScope (st, _, cls) scope =
  case scope of
    ExpressionNoop            -> return cls
    ExpressionId _ (Id name)  -> do
      n <- get name
      get $ getName n
      where get       = getNamedDecl st
            getName   = getIdString . getId . type'
    _ -> fail $
          msgWithToken' (getExprToken scope) "scope not a valid expression"
