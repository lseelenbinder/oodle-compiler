{-# LANGUAGE FlexibleInstances #-}

module Oodle.TypeChecker (typeChecker, typeOfExpression, getVarType) where

import Oodle.Error
import Oodle.ParseTree
import Oodle.SymbolTable
import Oodle.SymbolTableBuilder (getSymbolTable)
import Oodle.TreeWalker
import Oodle.Token (Token, fakeToken)

-- Check Types

typeChecker :: SymbolTable -> Start -> Error Type
typeChecker st = walk (st, (head st), (head st), False)
instance Walkable (Error Type) where
  reduce types = do
    t <- sequence types
    case t of
      []  -> yay
      _   -> return (head t)

  doClass     _ _ _ _ _ _ = yay
  doMethod  _ _ _ _ _ _ _ = yay
  doArgument      _ _ _ _ = yay
  doArgumentArr _ _ _ _ _ = yay
  doVariable (st, _, _, _) tk name t (_, exprT') = do
    exprT <- exprT'
    tExists <- tExists'
    if tExists && (t == exprT)
        || (exprT == typeNull)
        || (exprT == TypeNoop) then
      yay
    else
      fail (msgWithToken tk (wrongTypeMsg t exprT) name)
    where tExists' = case t of
                      (TypeId (Id typeName)) ->
                        case findSymbol st (typeName, tk) of
                          Ok _ -> return True
                          Failed _ ->
                            fail $ msgWithToken tk "undefined class" typeName
                      TypeNoop        -> return False
                      _               -> return True

  doAssignStmtArr scope tk name (arrayScope, arrayScope') (_, exprT') = do
    t <- getVarType scope (IdArray name arrayScope)
    arrayScope'' <- arrayScope'
    exprT <- exprT'
    if arrayScope'' == TypeInt then
      if t == exprT then
        yay
      else fail (msgWithToken' tk $ wrongTypeMsg t exprT)
    else fail (msgWithToken' tk $ wrongTypeMsg TypeInt arrayScope'')

  doAssignStmt scope tk name (_, expr) = do
    t <- getVarType scope (Id name)
    exprT <- expr

    let failMe = fail (msgWithToken' tk (wrongTypeMsg t exprT))

    if t == exprT then
      yay
    else
      case t of
        TypeId _ ->
          if isNullType exprT then yay
          else
            -- check if either side is a child of the other
            if isChildType scope t exprT || isChildType scope exprT t then
              yay
            else failMe
        _ -> failMe


  doIfStmt scope tk cond s _ = doLoopStmt scope tk cond s

  doLoopStmt _ tk (_, cond) _ = do
    cond' <- cond
    if cond' == TypeBoolean then
      yay
    else
      fail $ msgWithToken' tk "conditional not a boolean"

  doCallStmt scope tk (callScope, _) name (args, _) = do
    actualP <- mapM (doExpression scope) args
    methodCall scope tk callScope name actualP True

  doExpression = typeOfExpression

typeOfExpression :: Scope -> Expression -> Error Type
typeOfExpression scope e =
  case e of
    ExpressionInt _ _               -> return TypeInt
    ExpressionId _ i                -> getVarType scope i
    ExpressionStr _ _               -> return typeString
    ExpressionTrue _                -> return TypeBoolean
    ExpressionFalse _               -> return TypeBoolean
    ExpressionMe _                  -> return $ TypeId (Id className)
      where (_, c, _, _)  = scope
            className     = symbol c
    ExpressionNew _ t               -> return t
    ExpressionNull _                -> yay
    ExpressionNoop                  -> return TypeNoop

    ExpressionCall tk callScope (Id name) args -> do
      actualP <- mapM (doExpression scope) args
      methodCall scope tk callScope name actualP False

    -- TODO: Arrays
    ExpressionCall tk callScope (IdArray name _) args -> do
      actualP <- mapM (doExpression scope) args
      methodCall scope tk callScope name actualP False

    ExpressionIdArray _ i           -> getVarType scope i
    ExpressionNeg _ expr            -> checkTypeE' expr TypeInt
    ExpressionPos _ expr            -> checkTypeE' expr TypeInt
    ExpressionMul _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionDiv _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionAdd _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionSub _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionStrCat _ expr1 expr2  -> checkBothTypeE' expr1 expr2 typeString

    ExpressionEq _ expr1 expr2      ->
      checkTypeArray expr1 expr2 [typeString, TypeInt, TypeBoolean] >>
        return TypeBoolean
    ExpressionGt _ expr1 expr2      ->
      checkTypeArray expr1 expr2 [typeString, TypeInt] >> return TypeBoolean
    ExpressionGtEq _ expr1 expr2    ->
      checkTypeArray expr1 expr2 [typeString, TypeInt] >> return TypeBoolean

    ExpressionAnd _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeBoolean
    ExpressionOr _ expr1 expr2      -> checkBothTypeE' expr1 expr2 TypeBoolean
    ExpressionNot _ expr            -> checkTypeE' expr TypeBoolean
  where
    checkTypeE'         = checkTypeE scope
    checkBothTypeE'     = checkBothTypeE scope
    checkBothTypeMulE'  = checkBothTypeMulE scope
    checkTypeArray e1 e2 types = checkBothTypeMulE' e1 e2 types

getVarType :: Scope -> Id -> Error Type
getVarType (_, c, m, _) (Id name) = do
  d <- getNamedDecl
        (getSymbolTable ++ getVariables m ++ getVariables c)
        (name, fakeToken)
  return $ type' d
getVarType scope (IdArray name exprs) = do
  t <- getVarType scope (Id name)
  return $ unbuildArray t (length exprs)

yay :: Error Type
yay = return typeNull

methodCall ::
  Scope -> Token -> Expression -> String -> [Type] -> Bool -> Error Type
methodCall scope tk callScope name actualP returnNull = do
    callScope' <- resolveScope scope callScope
    method <- getMethodDecl callScope' (name, tk)
    let formalP = getParameterTypes method

    if length actualP == length formalP then
      if matchParameters actualP formalP then return
        (if returnNull then typeNull else (Oodle.SymbolTable.getType method))
      else
      fail $ msgWithToken' tk $
        "actual parameters do not match formal parameters ("
        ++ show actualP ++ " != " ++ show formalP ++ ")"
    else fail $ msgWithToken tk "incorrect number of parameters" name
  where matchParameters [] [] = True
        matchParameters ((TypeId t):actual) ((TypeId t'):formal) =
          -- check if either side is a child of the other
          isChildType scope (TypeId t) (TypeId t')
          || isChildType scope (TypeId t') (TypeId t)
          && matchParameters actual formal
        matchParameters (a:actual) (f:formal) =
          -- check primative types
          f == a
          && matchParameters actual formal

isChildType :: Scope -> Type -> Type -> Bool
isChildType _ (TypeId _) (TypeId (Id "")) = False
isChildType scope (TypeId (Id t)) (TypeId (Id t')) =
  t == t'
  || t == parent
  || isChildType scope (TypeId (Id t)) (TypeId (Id parent))
  where (st, _, _, _) = scope
        cls  = findKnownSymbol st t'
        (ClassDecl _ parent _ _ _) = decl cls
isChildType _ _ _ = False

checkTypeE :: Scope -> Expression -> Type -> Error Type
checkTypeE scope expr expectedT = do
  t <- doExpression scope expr
  if t == expectedT then
    return t
  else
    fail $ msgWithToken' (getExprToken expr) $ wrongTypeMsg expectedT t

checkBothTypeE :: Scope -> Expression -> Expression -> Type -> Error Type
checkBothTypeE scope e1 e2 t = do
  t' <- checkTypeE' e1 t
  checkTypeE' e2 t'
  where checkTypeE' = checkTypeE scope

checkBothTypeMulE :: Scope -> Expression -> Expression -> [Type] -> Error Type
checkBothTypeMulE scope e1 e2 [] = do
  e1' <- ctE e1
  e2' <- ctE e2
  if e1' == e2' then return e1'
  else
    fail $ msgWithToken' (getExprToken e1)
        "expression didn't match any of the expected types got: " ++ show e1'
        ++ " and " ++ show e2'
  where ctE = doExpression scope

checkBothTypeMulE scope e1 e2 (t:types) = do
  e1' <- ctE e1
  e2' <- ctE e2
  if t == e1' && t == e2'
  then
    return e1'
  else
    checkBothTypeMulE scope e1 e2 types
  where ctE = doExpression scope

wrongTypeMsg :: Type -> Type -> String
wrongTypeMsg expectedT t =
  "expected type: " ++ show expectedT ++ " got type: " ++ show t
