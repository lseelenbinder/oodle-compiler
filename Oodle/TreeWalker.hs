module Oodle.TreeWalker
  (unsupportedFeatures, symbolTableBuilder, typeChecker)
where

import Oodle.ParseTree
import Oodle.SymbolTable
import Oodle.Token
import Data.List (nub)

unsupportedFeatures :: Start -> [String]
unsupportedFeatures (Start classes) =
  reduceUF $ concatMap uwC classes

type UF = Maybe (Token, String)

reduceUF :: [UF] -> [String]
reduceUF [] = []
reduceUF (Nothing:xs) = reduceUF xs
reduceUF (Just (t, s):xs) =
  msg : reduceUF xs
  where msg = msgWithToken t "Unsupported feature" s

uwC :: Class -> [UF]
uwC (Class tk _ (Id parent) vars methods)
  | parent /= ""          = Just (tk, "class inheritance") : v ++ w
  | otherwise             = v ++ w
  where v = concatMap (uwV tk) vars
        w = concatMap uwM methods

uwM :: Method -> [UF]
uwM (Method tk (Id name) _ args vars statements)
  | name /= "start"       = Just (tk, "method other than 'start'") : t
  | otherwise             = t
  where a = map (uwA tk) args
        v = concatMap (uwV tk) vars
        s = concatMap uwS statements
        t = a ++ v ++ s

uwA :: Token -> Argument -> UF
uwA tk (Argument (IdArray _ exprs) _)
  | exprs /= []           = Just (tk, "array indexing")
  | otherwise             = Nothing
uwA tk (Argument _ t)
  | t == TypeString       = Just (tk, "string type argument")
  | otherwise             = Nothing

uwV :: Token -> Var -> [UF]
uwV tk (Var _ typ expr)
  | expr /= ExpressionNoop  = [Just (tk, "variable with assignment"), e]
  | isArrayType t           = [Just (tk, "array type variable"), e]
  | t == TypeString         = [Just (tk, "string type variable"), e]
  | otherwise               = [e]
  where e = uwE tk expr
        t = buildType typ

uwS :: Statement -> [UF]
uwS (AssignStatement tk (IdArray _ exprs) expr)
  | exprs /= []           = [Just (tk, "array indexing"), e]
  | otherwise             = [e]
  where e = uwE tk expr
uwS (IfStatement tk cond stmts stmts') = uwS' tk cond (stmts ++ stmts')
uwS (LoopStatement tk cond stmts) = uwS' tk cond stmts
uwS (CallStatement tk expr _ exprs) = map (uwE tk) (expr : exprs)

uwS' :: Token -> Expression -> [Statement] -> [UF]
uwS' tk expr stmts = e : s
  where e = uwE tk expr
        s = concatMap uwS stmts

uwE :: Token -> Expression -> UF
uwE tk' e =
  case e of
    ExpressionNew tk _ -> Just (tk, "new id expression")
    ExpressionMe tk    -> Just (tk, "me expression")
    ExpressionNull tk  -> Just (tk, "null expression")
    ExpressionStr _  -> Just (tk', "string literal")
    _                  -> Nothing

-- Build Symbol Table
symbolTableBuilder :: Start -> SymbolTable
symbolTableBuilder (Start classes) = mergeSymbolTable base c
  where base  = getSymbolTable
        c     = stC base classes

stC :: SymbolTable -> [Class] -> SymbolTable
stC st [] = st
stC st (Class tk (Id name) _ vars methods:cs) =
  if valid then stC (pushSymbol st c) cs
    else error $ msgWithToken tk "redeclared class" name
  where valid = verifyPush st name
        base  = pushClass name
        scope = [base [] []] -- initializes the Class scope
        v     = stV tk scope vars
        m     = stM v methods
        c     = base v m

stM :: [Symbol] -> [Method] -> [Symbol]
stM _ [] = []
stM mScope (Method tk (Id name) t args vars _:ms) =
  if valid then m : stM mScope' ms
    else error $ msgWithToken tk "redeclared method" name
  where valid   = verifyPush mScope name
        base    = pushMethod name (buildType t)
        a       = unzip . reverse $ stA args
        scope   = base (length (fst a)) (fst a) [] : snd a -- initializes the Method scope
        v       = stV tk scope vars
        m       = base (length (fst a)) (fst a) v
        mScope' = m : mScope

stV :: Token -> [Symbol] -> [Var] -> [Symbol]
stV _ scope [] = tail scope
stV tk scope (Var (Id name) t _:vs) =
  let
    stV' = stV tk
  in
    if valid then v : stV' scope' vs
      else error $ msgWithToken tk "redeclared variable" name
    where valid   = verifyPush scope name
          v       = pushVar name (buildType t)
          scope' = v : scope

stA :: [Argument] -> [(Type, Symbol)]
stA [] = []
stA (Argument (Id i) t:as) =
  (t, Symbol i (VarDecl t)) : stA as

typeChecker :: SymbolTable -> Start -> Bool
typeChecker st (Start classes) =
  all (tcC st) classes

tcC :: SymbolTable -> Class -> Bool
tcC st (Class tk (Id name) _ vars methods) =
  v && m
  where cd        = getClassDecl st name
        v         = all (tcV (st, cd, cd, tk)) vars
        m         = all (tcM st cd) methods

tcV :: Scope -> Var -> Bool
tcV (st, m, cls, tk) (Var (Id i) t expr) =
  (t == exprT) || (t == TypeNull) || (exprT == TypeNoop) ||
    error (msgWithToken tk (wrongTypeMsg t exprT) i)
  where exprT = calculateTypeE (st, m, cls, tk) expr

tcM :: SymbolTable -> Declaration -> Method -> Bool
tcM st cls (Method tk (Id name) _ _ vars stmts) =
  v && s
  where m = getMethodDecl cls name
        v = all (tcV (st, m, cls, tk)) vars
        s = all (tcS (st, m, cls, tk)) stmts

tcS :: Scope -> Statement -> Bool

tcS (st, m, cls, _) (AssignStatement tk (Id name) expr) =
  t == tId ||
  error (msgWithToken' tk $ wrongTypeMsg tId t)
  where tId = getVarType m cls (Id name)
        t = calculateTypeE (st, m, cls, tk) expr

tcS (st, m, cls, _) (AssignStatement tk (IdArray name exprs) expr) =
  if t == tId then
    TypeInt == e ||
    error (msgWithToken' tk $ wrongTypeMsg TypeInt e)
  else error $ msgWithToken' tk $ wrongTypeMsg tId t
  where tId = getVarType m cls (IdArray name exprs)
        t = calculateTypeE (st, m, cls, tk) expr
        e = if null exprs then TypeInt else head . nub $ map (calculateTypeE (st, m, cls, tk)) exprs

tcS (st, m, cls, _) (IfStatement tk cond stmts stmts') = tcS' (st, m, cls, tk) cond (stmts ++ stmts')

tcS (st, m, cls, _) (LoopStatement tk cond stmts) = tcS' (st, m, cls, tk) cond stmts

tcS (st, m, cls, _) (CallStatement tk scope (Id name) args) =
  if length actualP == length formalP then
    actualP == formalP ||
    error (msgWithToken tk ("actual parameters do not match formal parameters (" ++ show actualP ++ " !=  "++ show formalP) name)
  else error $ msgWithToken tk "incorrect number of parameters" name
  where scope' = resolveScope (st, m, cls, tk) scope
        method = getMethodDecl scope' name
        actualP = map (calculateTypeE (st, m, cls, tk)) args
        formalP = getParameterTypes method

tcS' :: Scope -> Expression -> [Statement] -> Bool
tcS' (st, m, cls, tk) expr stmts =
  if e == TypeBoolean then s
  else
    error $ msgWithToken' tk "conditional not a boolean"
  where e = calculateTypeE (st, m, cls, tk) expr
        s = all (tcS (st, m, cls, tk)) stmts

calculateTypeE :: Scope -> Expression -> Type
calculateTypeE (st, m, cls, tk) e =
  case e of
    ExpressionInt _                 -> TypeInt
    ExpressionId i                  -> getVarType m cls i
    ExpressionStr _                 -> TypeString
    ExpressionTrue _                -> TypeBoolean
    ExpressionFalse _               -> TypeBoolean
    ExpressionMe _                  -> TypeId (Id "me")
    ExpressionNew _ t               -> t
    ExpressionNull _                -> TypeNull
    ExpressionNoop                  -> TypeNoop

    -- The only expression that needs secondary verification
    ExpressionCall _ scope (Id name) args -> if valid then getType method else TypeNoop
      where method = getMethodDecl (resolveScope (st, m, cls, tk) scope) name
            valid = tcS (st, m, cls, tk) (CallStatement tk scope (Id name) args)

    ExpressionIdArray _ i           -> getVarType m cls i
    ExpressionNeg _ expr            -> checkTypeE' expr TypeInt
    ExpressionPos _ expr            -> checkTypeE' expr TypeInt
    ExpressionMul _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionDiv _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionAdd _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionSub _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeInt
    ExpressionStrCat _ expr1 expr2  -> checkBothTypeE' expr1 expr2 TypeString
    -- Todo
    ExpressionEq _ expr1 expr2      -> checkBothTypeMulE' expr1 expr2 [TypeString, TypeInt, TypeBoolean]
    ExpressionGt _ expr1 expr2      -> checkBothTypeMulE' expr1 expr2 [TypeString, TypeInt]
    ExpressionGtEq _ expr1 expr2    -> checkBothTypeMulE' expr1 expr2 [TypeString, TypeInt]
    ExpressionAnd _ expr1 expr2     -> checkBothTypeE' expr1 expr2 TypeBoolean
    ExpressionOr _ expr1 expr2      -> checkBothTypeE' expr1 expr2 TypeBoolean
    ExpressionNot _ expr            -> checkTypeE' expr TypeBoolean
  where
    checkTypeE'         = checkTypeE (st, m, cls, tk)
    checkBothTypeE'     = checkBothTypeE (st, m, cls, tk)
    checkBothTypeMulE'  = checkBothTypeMulE (st, m, cls, tk)

checkTypeE :: Scope -> Expression -> Type -> Type
checkTypeE (st, m, cls, tk) expr expectedT =
  if t == expectedT then t
  else error $ msgWithToken' tk $ wrongTypeMsg expectedT t
  where t = calculateTypeE (st, m, cls, tk) expr

checkBothTypeE :: Scope -> Expression -> Expression -> Type -> Type
checkBothTypeE (st, m, cls, tk) e1 e2 t = checkTypeE' e2 $ checkTypeE' e1 t
  where checkTypeE' = checkTypeE (st, m, cls, tk)

checkBothTypeMulE :: Scope -> Expression -> Expression -> [Type] -> Type
checkBothTypeMulE (_, _, _, tk) _ _ [] = error $ msgWithToken' tk "expression didn't match any of the expected types"
checkBothTypeMulE scope e1 e2 (t:types) =
  if t == e1' && t == e2' then t
  else checkBothTypeMulE scope e1 e2 types
  where
    ctE = calculateTypeE scope
    e1' = ctE e1
    e2' = ctE e2

msgWithToken' :: Token -> String -> String
msgWithToken' tk msg = init . init $ msgWithToken tk msg ""

msgWithToken :: Token -> String -> String -> String
msgWithToken tk msg name = printToken tk ++ ": " ++ msg ++ ": " ++ name

wrongTypeMsg :: Type -> Type -> String
wrongTypeMsg expectedT t = "expected type: " ++ show expectedT ++ " got type: " ++ show t

-- Handles building any kind of array type
buildType :: Type -> Type
buildType (TypeExp t _) = TypeArray (buildType t)
buildType t = t

unbuildArray :: Type -> Int -> Type
unbuildArray t 0 = t
unbuildArray (TypeArray t) i = unbuildArray t (i - 1)

getVarType :: Declaration -> Declaration -> Id -> Type
getVarType m cls (Id name) = type' $ getNamedDecl (getVariables m ++ getVariables cls) name
getVarType m cls (IdArray name exprs) = unbuildArray t (length exprs)
  where t =  getVarType m cls (Id name)

resolveScope :: Scope -> Expression -> Declaration
resolveScope (st, m, cls, tk) scope =
  if scope == ExpressionNoop then cls
  else error $ show $ getNamedDecl st (getIdString (getId class'))
  where class' = calculateTypeE (st, m, cls, tk) scope
