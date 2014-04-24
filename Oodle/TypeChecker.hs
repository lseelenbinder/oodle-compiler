{-# LANGUAGE FlexibleInstances #-}

module Oodle.TypeChecker (typeChecker, typeOfExpression) where

import Oodle.Error
import Oodle.ParseTree
import Oodle.SymbolTable (
  SymbolTable
  , findSymbol
  , resolveScope
  , getMethodDecl
  , getNamedDecl
  , getParameterTypes
  , getVariables
  , type'
  , unbuildArray
  , symbol
  , Scope)
import qualified Oodle.SymbolTable (getType)
import Oodle.SymbolTableBuilder (getSymbolTable)
import Oodle.TreeWalker
import Oodle.Token (Token, fakeToken)

-- Check Types
--

yay :: Error Type
yay = return typeNull

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
    if tExists && (t == exprT) || (exprT == typeNull) || (exprT == TypeNoop) then
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
        TypeId _  -> if exprT == typeNull then yay
                      else failMe
        _         -> failMe


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

  doExpression scope e =
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

methodCall :: Scope -> Token -> Expression -> String -> [Type] -> Bool -> Error Type
methodCall scope tk callScope name actualP returnNull = do
  callScope' <- resolveScope scope callScope
  method <- getMethodDecl callScope' (name, tk)
  let formalP = getParameterTypes method

  if length actualP == length formalP then
    if actualP == formalP then return
      (if returnNull then typeNull else (Oodle.SymbolTable.getType method))
    else
    fail (msgWithToken tk ("actual parameters do not match formal parameters ("
      ++ show actualP ++ " !=  "++ show formalP) name)
  else fail $ msgWithToken tk "incorrect number of parameters" name

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

getVarType :: Scope -> Id -> Error Type
getVarType (_, c, m, _) (Id name) = do
  d <- getNamedDecl (getSymbolTable ++ getVariables m ++ getVariables c) (name, fakeToken)
  return $ type' d
getVarType scope (IdArray name exprs) = do
  t <- getVarType scope (Id name)
  return $ unbuildArray t (length exprs)
