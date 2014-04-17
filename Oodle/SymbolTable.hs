{-# LANGUAGE FlexibleInstances #-}

module Oodle.SymbolTable
  (
    Symbol(..),
    SymbolTable,
    Declaration(..),
    Scope,
    findSymbol,
    isClassDecl,
    isMethodDecl,
    isVarDecl,
    getClassDecl,
    getMethodDecl,
    getNamedDecl,
    getType,
    getMethods,
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
    --                variables    methods
    = ClassDecl Token [Symbol]     [Symbol]
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
    ClassDecl _ vars _ -> vars
    MethodDecl _ _ _ _ vars -> vars

getMethods :: Symbol -> [Symbol]
getMethods (Symbol _ (ClassDecl _ _ methods)) = methods

-- Get a class declaration from a SymbolTable
getClassDecl :: SymbolTable -> String -> Error Declaration
getClassDecl = getNamedDecl

-- Get a method declaration from a parent class declaration
getMethodDecl :: Declaration -> String -> Error Declaration
getMethodDecl (ClassDecl _ _ methods) = getNamedDecl methods

getNamedDecl :: [Symbol] -> String -> Error Declaration
getNamedDecl symbols sym = do
  s <- findSymbol symbols sym
  return $ decl s

-- Symbol must exist
findSymbol :: [Symbol] -> String -> Error Symbol
findSymbol symbols sym =
  if not $ null match then return $ head match
  else fail $ show symbols ++ "\n" ++ "Undeclared variable/method: " ++ sym
  where match = dropWhile (\s -> symbol s /= sym) symbols

resolveScope :: Scope -> Expression -> Error Declaration
resolveScope (st, cls, _, _) scope =
  case scope of
    ExpressionNoop            -> return (decl cls)
    ExpressionId _ (Id name)  -> do
      n <- get name
      get $ getName n
      where get       = getNamedDecl st
            getName   = getIdString . getId . type'
    _ -> fail $
          msgWithToken' (getExprToken scope) "scope not a valid expression"

-- Handles building any kind of array type
buildType :: Type -> Type
buildType (TypeExp t _) = TypeArray (buildType t)
buildType t = t

unbuildArray :: Type -> Int -> Type
unbuildArray t 0 = t
unbuildArray (TypeArray t) i = unbuildArray t (i - 1)
