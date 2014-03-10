module Oodle.SymbolTable where

import Oodle.ParseTree
import Oodle.Token
import Data.List (union)

type SymbolTable = [Symbol]

mergeSymbolTable :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTable symbols symbols' = symbols `union` symbols'

data Symbol = Symbol {
  symbol :: String,
  decl :: Declaration
  }
  deriving (Show, Eq)

data Declaration
    --                variables    methods
    = ClassDecl       [Symbol]     [Symbol]
    --                parameters  parameter types   variables
    | MethodDecl Type Int         [Type]            [Symbol]
    | VarDecl    { type' :: Type }
    deriving (Show, Eq)

type Scope = (SymbolTable, Declaration, Declaration, Token)

getParameterTypes :: Declaration -> [Type]
getParameterTypes (MethodDecl _ _ types _) = types

getType :: Declaration -> Type
getType (MethodDecl t _ _ _) = t

getVariables :: Declaration -> [Symbol]
getVariables (ClassDecl vars _) = vars
getVariables (MethodDecl _ _ _ vars) = vars

getSymbolTable :: SymbolTable
getSymbolTable =
  [
  -- global Reader
  Symbol "Reader" (ClassDecl [] [Symbol "readInt" (MethodDecl TypeInt 0 [] [])]),
  Symbol "in" (VarDecl (TypeId (Id "Reader"))),
  -- global Writer
  Symbol "Writer" (ClassDecl [] [Symbol "writeInt" (MethodDecl TypeNull 1 [TypeInt] [])]),
  Symbol "out" (VarDecl (TypeId (Id "Writer")))
  ]

-- Get a class declaration from a SymbolTable
getClassDecl :: SymbolTable -> String -> Declaration
getClassDecl = getNamedDecl

-- Get a method declaration from a parent class declaration
getMethodDecl :: Declaration -> String -> Declaration
getMethodDecl (ClassDecl _ methods) = getNamedDecl methods

-- Get a var declaration from a parent method declaration
getVarDecl :: Declaration -> String -> Declaration
getVarDecl d = getNamedDecl (getVariables d)

getNamedDecl :: [Symbol] -> String -> Declaration
getNamedDecl symbols sym =
  decl (findSymbol symbols sym)

-- Symbol must exist
findSymbol :: [Symbol] -> String -> Symbol
findSymbol symbols sym =
  if not $ null match then head match
  else error $ "Undeclared variable/method: " ++ sym
  where match = dropWhile (\s -> symbol s /= sym) symbols

-- Class Declarations
pushClass :: String -> [Symbol] -> [Symbol] -> Symbol
pushClass sym var methods = Symbol sym (ClassDecl var methods)

-- Variable Declarations
pushVar :: String -> Type -> Symbol
pushVar sym t = Symbol sym (VarDecl t)

-- Method Declarations
pushMethod :: String -> Type -> Int -> [Type] -> [Symbol] -> Symbol
pushMethod sym t paramCount params vars =
  Symbol sym (MethodDecl t paramCount params vars)

-- Add a Symbol to a SymbolTable
pushSymbol :: SymbolTable -> Symbol -> SymbolTable
pushSymbol symbols s = symbols ++ [s]

-- Verify Any Push
verifyPush :: [Symbol] -> String -> Bool
verifyPush symbols sym = foldl (\x s -> x && symbol s /= sym) True symbols
