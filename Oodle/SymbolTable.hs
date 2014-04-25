{-# LANGUAGE FlexibleInstances #-}

module Oodle.SymbolTable
  (
    Symbol(..),
    SymbolTable,
    Declaration(..),
    Scope,
    findDecl,
    findSymbol,
    isClassDecl,
    isMethodDecl,
    isVarDecl,
    getClassDecl,
    getMethodDecl,
    getNamedDecl,
    getType,
    getMethods,
    getInheritedMethods,
    getVariables,
    getParameterTypes,
    resolveScope,
    buildType,
    unbuildArray
    )
where

import Oodle.Error
import Oodle.ParseTree
import Oodle.Token

type SymbolTable = [Symbol]

data Symbol = Symbol {
  symbol :: String,
  decl :: Declaration
  }
  deriving (Show, Eq)

data Declaration
    --                parent variables    methods  inherited methods
    = ClassDecl Token String [Symbol]     [Symbol] [(String, Symbol)]
    --                      parameters  parameter types   variables
    | MethodDecl Token Type Int         [Type]            [Symbol]
    | VarDecl    { varToken :: Token, type' :: Type }
    deriving (Show, Eq)

--                         Class   Method  Debug
type Scope = (SymbolTable, Symbol, Symbol, Bool)

isVarDecl :: Symbol -> Bool
isVarDecl (Symbol _ VarDecl{}) = True
isVarDecl _ = False

isClassDecl :: Symbol -> Bool
isClassDecl (Symbol _ ClassDecl{}) = True
isClassDecl _ = False

isMethodDecl :: Symbol -> Bool
isMethodDecl (Symbol _ MethodDecl{}) = True
isMethodDecl _ = False

getParameterTypes :: Declaration -> [Type]
getParameterTypes (MethodDecl _ _ _ types _) = types

getType :: Declaration -> Type
getType (MethodDecl _ t _ _ _) = t

getVariables :: Symbol -> [Symbol]
getVariables sym =
  case decl sym of
    ClassDecl _ _ vars _ _ -> vars
    MethodDecl _ _ _ _ vars -> vars

getMethods :: Symbol -> [Symbol]
getMethods (Symbol _ (ClassDecl _ _ _ methods _)) = methods

getInheritedMethods :: Symbol -> [(String, Symbol)]
getInheritedMethods (Symbol _ (ClassDecl _ _ _ _ methods)) = methods

-- Get a class declaration from a SymbolTable
getClassDecl :: SymbolTable -> (String, Token) -> Error Declaration
getClassDecl = getNamedDecl

-- Get a method declaration from a parent class declaration
getMethodDecl :: Declaration -> (String, Token) -> Error Declaration
getMethodDecl (ClassDecl _ _ _ methods inherited) = getNamedDecl (methods ++ snd (unzip inherited))

getNamedDecl :: [Symbol] -> (String, Token) -> Error Declaration
getNamedDecl symbols sym = do
  s <- findSymbol symbols sym
  return $ decl s

-- Symbol must exist
findSymbol :: [Symbol] -> (String, Token) -> Error Symbol
findSymbol symbols (sym, tk) =
  if not $ null match then return $ head match
  else
    fail $ concatMap (\s -> show s ++ "\n") symbols ++
      msgWithToken tk "undeclared variable/method" sym
  where match = dropWhile (\s -> symbol s /= sym) symbols

-- Symbol must exist
findDecl :: [Symbol] -> (Declaration, Token) -> Error Symbol
findDecl symbols (d, tk) =
  if not $ null match then return $ head match
  else
    fail $ concatMap (\s -> show s ++ "\n") symbols ++
      "\n" ++ (msgWithToken tk "couldn't find declaration" (show d))
  where match = dropWhile (\s -> decl s /= d) symbols

resolveScope :: Scope -> Expression -> Error Declaration
resolveScope (st, cls, m, d) scope =
  case scope of
    ExpressionNoop            -> return (decl cls)

    ExpressionId tk (Id name) -> do
      n <- get (name, tk)
      get (getName n, tk)
      where get       = getNamedDecl (getVariables m ++ getVariables cls ++ st)
            getName   = getIdString . getId . type'

    ExpressionNew tk (TypeId (Id name)) -> getNamedDecl st (name, tk)

    ExpressionCall tk callScope (Id name) _ -> do
      callScope' <- resolveScope (st, cls, m, d) callScope
      (MethodDecl _ (TypeId (Id t)) _ _ _) <- getMethodDecl callScope' (name, tk)
      getDeclFromType t

    ExpressionStr _ _ ->
      getDeclFromType "String"

    ExpressionMe _ ->
      return $ decl cls

    _ -> fail $
            msgWithToken' (getExprToken scope) "scope not a valid expression"
    where
      getDeclFromType t =
        return (decl (deE (findSymbol st (t, fakeToken))))

-- Handles building any kind of array type
buildType :: Type -> Type
buildType (TypeExp t _) = TypeArray (buildType t)
buildType t = t

unbuildArray :: Type -> Int -> Type
unbuildArray t 0 = t
unbuildArray (TypeArray t) i = unbuildArray t (i - 1)
